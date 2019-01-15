module Lib where


import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe                   (fromJust)
import           Data.String.Conv             (toS)
import           Data.Text                    (Text)
import           Network.SSH.Client.SimpleSSH
import           System.IO                    (hClose)
import           System.Posix.Temp            (mkstemp)
import           Text.Read
import qualified Codec.Crypto.RSA.Pure        as RSA
import qualified Data.ByteString.Base64.Lazy  as Base64
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as C
import qualified Data.Aeson as Aeson

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


dispatchCommandTo 
  :: Node
  -> MasterCommand
  -> MasterApp (Either String String)

dispatchCommandTo nd mc = do

  pkpath <- asksApp signPKey
  pkey <- read . toS <$> liftIO (B.readFile pkpath)
  let cmdstr = Aeson.encode mc
  case RSA.sign pkey cmdstr of
    Left e -> pure $ Left ("rsa error: " ++ (show e))
    Right sig ->
      let sig' = Base64.encode sig
          tosend = Aeson.encode (toS cmdstr :: Text, toS sig' :: Text)
       in send tosend >>= \case
            Left e -> pure $ Left ("ssh error: " ++ show e)
            Right v -> pure $ parseResult v

  where

    send str = do
      path <- liftIO $ putInTemp str
      liftIO $ putStrLn curl
      execOn nd $ \s -> 
        (sendFile s 0o600 path "/tmp/last.cmd.txt" >> execCommand s curl)

    curl = "curl http://localhost:3333/ \
           \--header \"Content-type: application/json\" \
           \-X POST --data @/tmp/last.cmd.txt"

    putInTemp str = do
      (path, h) <- mkstemp "/tmp/sshd"
      C.hPut h str >> hClose h >> return path

    parseResult r =
      case resultExit r of
        ExitSuccess -> Right $ toS $ resultOut r
        _           -> Left $ "curl failed: " ++ show r


testWorld :: App ClusterState MasterConfig b -> IO b
testWorld r = do
  (world, tcs) <- buildInitConfig "./assets/config.yaml"
  runApp world r


testCmd ::
  MasterCommand
  -> App ClusterState MasterConfig (Either String String)
testCmd k = do
  nd <- asksApp nodes
  dispatchCommandTo (nd!!2) k
