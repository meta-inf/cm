{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where


import qualified Codec.Crypto.RSA.Pure        as RSA
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception            (bracket, catch)
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
import           Data.Either
import           Data.String.Conv             (toS)
import           Data.Text                    (Text)
import           Data.Text.Lazy.Builder       (toLazyText)
import           Data.Time.Clock              (UTCTime)
import           Katip                        (Severity (..), logTM, ls, showLS)
import qualified Katip                        as K
import           Language.Haskell.TH.Syntax   (Loc (..))
import           Network.SSH.Client.SimpleSSH hiding (ResultExit (..))
import qualified Network.SSH.Client.SimpleSSH as SSH
import           System.Exit                  (ExitCode (..))
import           System.IO                    (hClose, stdout)
import           System.IO.Error              (IOError)
import           System.FilePath.Posix        (takeFileName, (</>))
import qualified System.Posix.Directory
import qualified System.Posix.Temp            as Temp
import           System.Process               (readProcess,
                                               readProcessWithExitCode)
import qualified System.Process               as Proc
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

data MasterMutable = MasterMutable
  { clusterState :: [(String, QueryResult)]
  , lastLogs     :: [(UTCTime, String, String)]
  }

type MasterApp = App MasterMutable MasterConfig


getClusterState :: MasterApp [(String, QueryResult)]
getClusterState = clusterState <$> getState

mutateClusterState
  :: ([(String, QueryResult)] -> [(String, QueryResult)])
     -> MasterApp ()
mutateClusterState f = mutateState f1
  where f1 v = v { clusterState = f (clusterState v) }


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


sshResultToEither 
  :: Either SimpleSSHError SSH.Result
  -> Either String (String, String)
sshResultToEither = \case
  Left se -> Left ("ssh connection error: " ++ show se)
  Right r -> case resultExit r of
               SSH.ExitSuccess -> Right (toS $ resultOut r, toS $ resultErr r)
               _ -> Left ("remote execution failed: " ++ show r)


queryNode :: Node -> MasterApp (Either String [GpuInfo])
queryNode nd = execOn nd action `for` \case
    Left err     -> returnErr $ "ssh connection failed: " ++ show err
    Right result -> 
      case resultExit result of
           SSH.ExitSuccess -> Right $ parseNvidiaSMI $ resultOut result
           _               -> returnErr $ "nvidia-smi failed: " ++ show result
  where
    action = \s -> execCommand s queryCmd
    returnErr s = Left s
    queryCmd = "nvidia-smi --query-gpu=name,index,memory.free,utilization.gpu \
      \--format=csv,nounits,noheader"


queryThread :: Int -> MasterApp ()
queryThread delay = do
  world <- askWorld
  let maintainSingleNode nd = forever do
        lastResultList <- getClusterState
        let nId = nodeName nd
            infoFrom res = (lastSuccessTime res, lastSuccessRes res)
            (rTime, rVal) = infoFrom . fromJust $ lookup nId lastResultList
        newRes <- queryNode nd
        cTime <- now
        let newRes' = case newRes of
                        Left err  -> QueryResult (Failure err) cTime rVal rTime
                        Right res -> QueryResult Success       cTime res  cTime
        mutateClusterState $ \curList -> assocListReplace nId newRes' curList
        delayBy delay
  let msn = runApp world . maintainSingleNode
  nodeList <- asksApp nodes
  $(logTM) InfoS "Deploying Query threads"
  liftIO $ mapConcurrently_ msn nodeList


initMutable :: [Node] -> MasterMutable
initMutable ns = 
  MasterMutable { clusterState = [(nodeName nd, def) | nd <- ns]
                , lastLogs = []
                }
  where def = QueryResult (Failure "Empty") defaultTime [] defaultTime


withInitConfig ::
  FilePath
  -> ((AppConfig MasterMutable MasterConfig, TVar MasterMutable)
      -> IO b)
  -> IO b
withInitConfig cpath k = do
  cfg <- loadConfig cpath
  storage <- newTVarIO (initMutable (nodes cfg))
  -- log scribes
  stdoutScribe <- K.mkHandleScribe (K.ColorLog True) stdout K.DebugS K.V1
  let myScribe = K.Scribe (myLiPush storage) (pure ())
      mkLogEnv =
        K.initLogEnv "Master" "devel" >>=
        K.registerScribe "stdout" stdoutScribe K.defaultScribeSettings >>=
        K.registerScribe "webp" myScribe K.defaultScribeSettings
  bracket mkLogEnv K.closeScribes $ \le -> 
    let args = (initConfig le storage cfg, storage)
     in k args
  where
    myLiPush storage it =
      let vs = (K._itemTime it,
                showLoc $ K._itemLoc it,
                toS $ toLazyText . K.unLogStr $ K._itemMessage it)
       in atomically do
         st <- readTVar storage
         writeTVar storage (st { lastLogs = vs : (lastLogs st) })
    showLoc Nothing = "N/A"
    showLoc (Just (Loc { loc_filename = lf, loc_start = cp })) =
      lf ++ ": " ++ show cp


withMasterThreads
  :: FilePath -> (TVar MasterMutable -> IO b) -> IO b
withMasterThreads cpath k =
  withInitConfig cpath \(world, storage) -> do
    forkIO $ runApp world (queryThread 500)
    k storage


data SlaveCheckResult = OutdatedSlaveRunning
                      | CheckErr String 
                      | CheckSuccess Int
                      deriving (Show)


checkIfSlaveOn :: Node -> MasterApp SlaveCheckResult
checkIfSlaveOn nd = do
  lhr <- liftIO (readProcess "sha1sum" [local_path] "")
  let localHash = head (words lhr)
  sshret <- execOn nd $ \sess -> do
    r1 <- execCommand sess (
      printf "sha1sum %s || echo \"nothing forsplit\"" remote_path)
    r2 <- execCommand sess ("ps -ux | grep -v grep | grep " ++ slave_name)
    -- NOTE should let slave save port there
    port <- parsePort sess
    let remoteHash = head $ words $ toS $ resultOut r1
    if resultExit r2 == SSH.ExitSuccess
       then if remoteHash /= localHash
               then return OutdatedSlaveRunning
               else return port
       else do
         when (remoteHash /= localHash) do
           -- upload the good slave
           execCommand sess "mkdir -p ~/slave/assets"
           sendFile sess 0o700 local_path remote_path
           sendFile sess 0o600 "./assets/pub.key" "slave/assets/pub.key" 
           pure ()
         -- launch it
         r <- execCommand sess ("nohup " ++ remote_path ++ " >/tmp/slave.log 2>&1 &")
         -- NOTE should wait until port number is announced
         case resultExit r of
           SSH.ExitSuccess -> parsePort sess
           SSH.ExitFailure c -> 
             return $ CheckErr ("slave launch failed: " ++ show r)
  return $ case sshret of
             Left e -> CheckErr ("ssh err: " ++ show e)
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
    remote_path = "slave/" ++ slave_name


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
        SSH.ExitSuccess -> pure (resultOut r)
        _ -> Exc.throwError ("execution on remote failed" ++ show r)
    curl = printf ("curl http://localhost:%d/ \
                   \--header \"Content-type: application/json\" \
                   \-X POST --data @/tmp/last.cmd.txt")
    putInTemp str = do
      (path, h) <- Temp.mkstemp "/tmp/sshd"
      C.hPut h str >> hClose h >> return path
    parseResult r =
      case resultExit r of
        SSH.ExitSuccess -> Right $ toS $ resultOut r
        _               -> Left $ "execution failed on slave: " ++ show r


implementPrereq 
  :: String -> [Node] -> Prerequisite -> MasterApp (Either String ())
implementPrereq logName nodes (SyncData local remote) = runExceptT do
  pubkey <- Exc.lift $ asksApp credential `for` publicKey
  rlst <- liftIO $ forM nodes $ \nd ->
    let sshcmd = printf "\"ssh -i %s -p %d\"" pubkey (port nd)
        rpath = printf "%s@%s" (userName nd) (hostName nd)
     in readProcessWithExitCode "rsync" ["-e", sshcmd, "-avv", local, rpath] ""
  let failed = filter (\(e,_,_) -> e /= ExitSuccess) rlst
  if not (null failed)
     then throwError $
            printf "prereq rsync %s -> %s failed on %d nodes: sample run %s"
            local remote (length failed) (unlines $ map show failed)
     else return ()
--
implementPrereq logName nodes (GitRepoIn local remoteDir commit) = runExceptT do
  -- tar our repo
  let tarOp = withTempDir $ \fp -> do
        readProcess "git" ["clone", local, fp] ""
        (ef, h) <- Temp.mkstemps "/tmp/sshd-repo" ".tgz"
        hClose h
        let prc = (Proc.shell ("git checkout " ++ commit)) {
            Proc.cwd = Just "/tmp/sshd-repo" }
        Proc.readCreateProcess prc ""
        readProcess "tar" ["zcvf", ef, fp] ""
        return (Right ef)
  tarPath' <- liftIO $
    catch tarOp (\e -> (pure $ Left $ "Tar failed: " ++ (show (e :: IOError))))
  tarPath <- liftEither tarPath'
  -- copy tarball
  let remoteTarPath = "/tmp" </> (takeFileName tarPath)
      remoteCmd = 
        printf "rm -rf %s && tar zxvf %s %s" remoteDir remoteTarPath remoteDir
      copyOp sess = do
        sendFile sess 0o600 tarPath remoteTarPath
        execCommand sess remoteCmd
  w <- Exc.lift askWorld
  rm <- (liftIO . forConcurrently nodes) $ \d -> runApp w (execOn d copyOp)
  let errs = lefts $ map sshResultToEither rm
  when (not (null errs)) $ throwError (unlines errs)
  return ()
  where
    withTempDir = bracket (Temp.mkdtemp "/tmp/sshd-repo")
                  System.Posix.Directory.removeDirectory

implementPrereq logName nodes (RunScript scriptPath) = do
  let rpath = "/tmp" </> (takeFileName scriptPath)
      rcmd = printf "script -qfc \"bash %s\" /tmp/slv.last.%s" rpath logName
      remoteOp sess = do
        sendFile sess 0o600 scriptPath rpath
        execCommand sess rcmd
  w <- askWorld
  r <- (liftIO . forConcurrently nodes) $ \d -> runApp w (execOn d remoteOp)
  let errs = lefts $ map sshResultToEither r
  return if (not (null errs)) then Left (unlines errs) else Right ()


-- |name of the taskGroup must be unique
launchTaskGroup :: TaskGroup -> [Task] -> [Node] -> MasterApp (Either String ())
launchTaskGroup tg ts nodes = runExceptT do
  let (TaskGroup preqs name) = tg
  -- 
  errs <- lefts <$> (Exc.lift $ mapM (implementPrereq name nodes) preqs)
  when (not (null errs)) $
    throwError ("cannot implement prereqs: " ++ unlines errs)
  -- 
  errs <- lefts <$> (
    Exc.lift $ mapM (flip dispatchCommandTo (ReportStatus)) nodes)
  when (not (null errs)) $
    throwError ("communication failure: " ++ unlines errs)
  -- 
  rs <- Exc.lift $ mapM (flip dispatchCommandTo (LaunchTask ts)) nodes
  let errs = lefts rs
  when (not (null errs)) $
    throwError ("dispatch command failure: " ++ unlines errs)

  return ()
  
-- 
-- for debug
--
buildInitConfig 
  :: FilePath -> IO (AppConfig MasterMutable MasterConfig, TVar MasterMutable)
buildInitConfig cpath = do
  cfg <- loadConfig cpath
  storage <- newTVarIO (initMutable (nodes cfg))
  return (initConfig undefined storage cfg, storage)


testWorld :: MasterApp b -> IO b
testWorld r = do
  (world, tcs) <- buildInitConfig "./assets/config.yaml"
  runApp world r


testCmd :: MasterCommand -> MasterApp (Either String Text)
testCmd k = do
  nd <- asksApp nodes
  dispatchCommandTo (nd!!2) k
