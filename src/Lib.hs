module Lib where


import qualified Codec.Crypto.RSA.Pure        as RSA
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Except         (liftEither, runExceptT,
                                               throwError, withExceptT)
import qualified Control.Monad.Except         as Exc
import           Control.Monad.IO.Class
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Base64.Lazy  as Base64
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as C
import           Data.Maybe                   (fromJust)
import           Data.String.Conv             (toS)
import           Data.Text                    (Text)
import           Network.SSH.Client.SimpleSSH
import           System.IO                    (hClose)
import           System.Posix.Temp            (mkstemp)
import           System.Process               (readProcess)
import           Text.Printf                  (printf)
import           Text.Read

import App
import Utils
import Config
import Task


data QueryStatus = Success | Failure String deriving (Show)

data QueryResult = QueryResult { queryStatus     :: QueryStatus
                               , queryTime       :: TimeStr
                               , lastSuccessRes  :: [GpuInfo]
                               , lastSuccessTime :: TimeStr
                               } deriving (Show)


type ClusterState = [(String, QueryResult)]

type MasterApp = App ClusterState MasterConfig


parseNvidiaSMI :: B.ByteString -> [GpuInfo]
parseNvidiaSMI stdout = [x | Just x <- map fromLine lines]
  where
    lines = B.lines stdout
    fromLine line = do
      let toks = map B.unpack $ B.split ',' line
      name <- pure $ toks !! 0
      id_  <- readMaybe $ toks !! 1
      fmem <- readMaybe $ toks !! 2
      util <- readMaybe $ toks !! 3
      return $ GpuInfo id_ name fmem util


execOn
  :: Node
  -> (Session -> SimpleSSH a)
  -> MasterApp (Either SimpleSSHError a)
execOn nd action = do
  crd <- asksApp credential
  kh <- asksApp knownHosts
  let app = (withSessionKey (hostName nd) (port nd) kh (userName nd)
             (publicKey crd) (privateKey crd) "") action
   in liftIO $ runSimpleSSH app


queryNode :: Node -> MasterApp (Either String [GpuInfo])
queryNode nd = execOn nd action `for` \case
    Left err     -> returnErr $ "ssh connection failed: " ++ show err
    Right result -> 
      case resultExit result of
           ExitSuccess -> Right $ parseNvidiaSMI $ resultOut result
           _           -> returnErr $ "nvidia-smi failed: " ++ show result
  where
    action = \s -> execCommand s queryCmd
    returnErr s = Left s
    queryCmd = "nvidia-smi --query-gpu=name,index,memory.free,utilization.gpu \
      \--format=csv,nounits,noheader"


initState :: [Node] -> ClusterState
initState = map $ \nd -> (nodeName nd, def)
  where def = QueryResult (Failure "Empty") defaultTime [] defaultTime


queryThread :: Int -> MasterApp ()
queryThread delay = do
  world <- askWorld
  let maintainSingleNode nd = forever do
        lastResultList <- getState
        let nId = nodeName nd
            infoFrom res = (lastSuccessTime res, lastSuccessRes res)
            (rTime, rVal) = infoFrom . fromJust $ lookup nId lastResultList
        newRes <- queryNode nd
        cTime <- now
        let newRes' = case newRes of
                        Left err  -> QueryResult (Failure err) cTime rVal rTime
                        Right res -> QueryResult Success       cTime res  cTime
        mutateState $ \curList -> assocListReplace nId newRes' curList
        delayBy delay
  let msn = runApp world . maintainSingleNode
  nodeList <- asksApp nodes
  liftIO $ mapConcurrently_ msn nodeList


buildInitConfig 
  :: FilePath -> IO (AppConfig ClusterState MasterConfig, TVar ClusterState)
buildInitConfig cpath = do
  cfg <- loadConfig cpath
  storage <- newTVarIO (initState (nodes cfg))
  return (initConfig undefined storage cfg, storage)


launchMasterThreads :: FilePath -> IO (TVar ClusterState)
launchMasterThreads cpath = do
  (s, storage) <- buildInitConfig cpath
  forkIO $ runApp s $ queryThread 500
  return storage


data SlaveCheckResult = OutdatedSlaveRunning
                      | CheckErr String 
                      | CheckSuccess Int
                      deriving (Show)


checkIfSlaveOn :: Node -> MasterApp SlaveCheckResult
checkIfSlaveOn nd = do
  lhr <- liftIO (readProcess "sha1sum" [local_path] "")
  let localHash = toS $ head (words lhr)
  sshret <- execOn nd $ \sess -> do
    r1 <- execCommand sess (
      printf "(sha1sum %s | awk '{print $1}') || echo \"nothing\"" remote_path)
    r2 <- execCommand sess ("ps -ux | grep -v grep | grep " ++ slave_name)
    -- TODO let slave save port there
    port <- parsePort sess
    if resultExit r2 == ExitSuccess
       then if resultOut r1 /= localHash
               then return OutdatedSlaveRunning
               else return port
       else do
         when (resultOut r1 /= localHash) do
           -- upload the good slave
           execCommand sess "mkdir -p ~/slave/assets"
           sendFile sess 0o700 remote_path local_path
           sendFile sess 0o700 "~/slave/assets/pub.key" "./assets/pub.key"
           pure ()
         -- launch it
         r <- execCommand sess ("nohup " ++ remote_path ++ " &")
         -- TODO should wait until port number is announced
         case resultExit r of
           ExitSuccess -> parsePort sess
           ExitFailure c -> 
             return $ CheckErr ("slave launch failed: " ++ show r)
  return $ case sshret of
             Left e -> CheckErr ("ssh err" ++ show e)
             Right v -> v
  where
    parsePort sess = return $ CheckSuccess 3333 {- do
      r3 <- exc sess "cat ~/slave/port"
      let r = case resultExit r3 of
                ExitFailure _ -> Nothing
                ExitSuccess -> readMaybe (resultOut r3) 
       in case r of
            Nothing -> CheckError "cannot find port"
            Just p -> CheckSuccess p -}
    slave_name = "slave-exe-x86_64.AppImage" :: String
    local_path = "./assets/" ++ slave_name
    remote_path = "~/slave/" ++ slave_name


dispatchCommandTo 
  :: Node
  -> MasterCommand
  -> MasterApp (Either String Text)
dispatchCommandTo nd mc = do
  pkpath <- asksApp signPKey
  pkey <- read . toS <$> liftIO (B.readFile pkpath)
  let cmdstr = Aeson.encode mc
  runExceptT do
    sig <- withExceptT show (liftEither $ RSA.sign pkey cmdstr)
    let sig' = Base64.encode sig
        tosend = Aeson.encode (toS cmdstr :: Text, toS sig' :: Text)
    rsp <- Exc.lift $ checkIfSlaveOn nd
    port <- case rsp of
      CheckSuccess p -> pure p
      _ -> throwError (show rsp)
    cmdpath <- liftIO $ putInTemp tosend
    r1 <- doSSH nd $ \s -> do
      sendFile s 0o600 cmdpath "/tmp/last.cmd.txt"
      execCommand s (curl port)
    return (toS r1)
  where
    doSSH nd cmd = do
      rraw <- Exc.lift (execOn nd cmd)
      r <- withExceptT show (Exc.liftEither rraw)
      case resultExit r of
        ExitSuccess -> pure (resultOut r)
        _ -> Exc.throwError ("execution on remote failed" ++ show r)
    curl = printf ("curl http://localhost:%d/ \
                   \--header \"Content-type: application/json\" \
                   \-X POST --data @/tmp/last.cmd.txt")
    putInTemp str = do
      (path, h) <- mkstemp "/tmp/sshd"
      C.hPut h str >> hClose h >> return path
    parseResult r =
      case resultExit r of
        ExitSuccess -> Right $ toS $ resultOut r
        _           -> Left $ "execution failed on slave: " ++ show r


testWorld :: App ClusterState MasterConfig b -> IO b
testWorld r = do
  (world, tcs) <- buildInitConfig "./assets/config.yaml"
  runApp world r


testCmd ::
  MasterCommand
  -> App ClusterState MasterConfig (Either String Text)
testCmd k = do
  nd <- asksApp nodes
  dispatchCommandTo (nd!!2) k
