{-# LANGUAGE TemplateHaskell #-}

module LocalRunner where


import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception         (bracket)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.List                 (sortBy)
import           Data.String.Conv          (toS)
import           Katip                     (Severity (..), logTM, ls, showLS)
import           System.Environment        (getExecutablePath)
import           System.Exit               (ExitCode (..))
import           System.FilePath.Posix     (takeDirectory, (</>))
import           System.IO                 (stdout)
import           System.Process
import           Text.Read                 (readMaybe)

import qualified Codec.Crypto.RSA.Pure     as RSA
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as B
import qualified Data.Text                 as Text
import qualified Katip                     as K
import qualified Network.HTTP.Types.Status as S
import qualified Network.Wai               as Wai
import qualified Network.Wai.Handler.Warp  as Warp

import           App
import           Config (GpuInfo (..))
import           Lib    (parseNvidiaSMI)
import           Task
import           Utils


type DeviceId = Int
type WorkerId = Int

data State = State
  { workerIds      :: [(DeviceId, [(WorkerId, ThreadId)])]
  , completedTasks :: [(Task, ExitCode)]
  , pendingTasks   :: [Task]
  }

type LocalApp = App State RSA.PublicKey

type LocalConfig = AppConfig State RSA.PublicKey

askKey :: LocalApp RSA.PublicKey
askKey = askApp


execTask :: MonadIO m => Task -> DeviceId -> m ExitCode
execTask task gpuId = liftIO $
  withCreateProcess cp $ \_ _ _ ph -> waitForProcess ph
  where
    cp = (proc (command task) (args task)) {
      cwd = Just (workDir task :: FilePath)
    , env = Just ev }
    ev = [("CUDA_VISIBLE_DEVICES", show gpuId)]


runnerThread :: DeviceId -> WorkerId -> LocalApp ()
runnerThread devId workerId = 
  K.katipAddContext (K.sl "worker" (devId, workerId)) loop
  where
    loop = do
      -- fetch next task
      nextTask <- liftSTM $ \storage -> do
        st <- readTVar storage
        let plist = pendingTasks st
        if null plist
          then return Nothing
          else do
            writeTVar storage (st { pendingTasks = tail plist })
            return $ Just (head plist)
      -- launch
      case nextTask of
        Nothing -> delayBy 1000 >> loop
        Just t  -> do
          $(logTM) InfoS ("Launching task " <> showLS t)
          exc <- execTask t devId
          $(logTM) InfoS ("Task finished, exit code " <> showLS exc)
          mutateState $ \st -> newState st t exc
    -- 
    newState st t exitCode =
      let State{completedTasks=ct, pendingTasks=pt} = st
          t' = t { retryCnt = (retryCnt t - 1) }
       in case exitCode of
            ExitFailure _ | (retryCnt t) > 0 -> st { pendingTasks = pt ++ [t'] }
            otherwise -> st { completedTasks = (t, exitCode) : ct }


findGPUs :: Int -> LocalApp [DeviceId]
findGPUs nGpu = do
  infos <- parseNvidiaSMI . C.pack <$> liftIO (readCreateProcess queryCmd "")
  let infos' = take nGpu $ sortBy cmp infos
  return $ map toId infos'
  where
    toId (GpuInfo i _ _ _) = i :: DeviceId
    cmp (GpuInfo _ _ fm1 _) (GpuInfo _ _ fm2 _) = compare fm2 fm1
    queryCmd = proc "nvidia-smi" [
      "--query-gpu=name,index,memory.free,utilization.gpu",
      "--format=csv,nounits,noheader"]


adjustResLimit :: Int -> Int -> LocalApp ()
adjustResLimit nDev nWorkerPerDev = do
  st <- getState
  let workers = workerIds st
  -- Kill workers on extra devices
  workers <- let (hd, tl) = splitAt nDev workers
              in forM tl (truncateThreads 0) >> pure hd
  -- Truncate nWorkerPerDev
  workers <- forM workers $ truncateThreads nWorkerPerDev
  -- Extend worker pools corresponding to existing GPUs
  oldWorkers <- forM workers $ \(d, ws) -> do
    let wid = foldr max 0 (map fst ws)
    nth <- newThreadList d [wid + 1 .. wid + (nWorkerPerDev - length ws)]
    return (d, ws ++ nth)
  -- Deploy workers on extra devs
  extraDevs <- findGPUs (nDev - length workers)
  $(logTM) K.DebugS ("New workers will go to " <> showLS extraDevs)
  newWorkers <- forM extraDevs $ \d -> do
    nth <- newThreadList d [1..nWorkerPerDev]
    return (d, nth)
  -- Log changes
  let workers = oldWorkers ++ newWorkers
  mutateState $ \st -> st { workerIds = workers }
  $(logTM) InfoS ("resource limit adjusted to " <> showLS (nDev, nWorkerPerDev))

  where
    truncateThreads nKeep (devId, tList) = do
      forM (drop nKeep tList) (liftIO . killThread . snd)
      return (devId, take nKeep tList)
    newThreadList devId workerIdRange = forM workerIdRange $ \wId -> do
      action <- toIO $ runnerThread devId wId
      tId <- liftIO (forkIO action)
      return (wId, tId)


reportStatus :: State -> B.ByteString
reportStatus st = encode $ SlaveStatus (pendingTasks st) (k su) (k fa) th
  where
    k = map fst
    (fa, su) = break (\(_, t) -> t == ExitSuccess) (completedTasks st)
    th = [(i, show j) | (i, j) <- workerIds st]


webApp :: LocalConfig -> Wai.Application
webApp cfg req _respond = runApp cfg $ do
  reqBody' <- liftIO $ getBody req C.empty
  let reqBody = toS reqBody' -- we've gone beyond GHC's capability
  let reqPath = map Text.unpack (Wai.pathInfo req)

  $(logTM) InfoS "request received"

  verify <- askKey `for` RSA.verify
  ret <- case parseCommand reqBody verify of
    Left err -> pure $ Left $ "parse failed" ++ err
    Right ReportStatus -> doReport
    Right (AdjustResLimit a b) -> doAdjust a b
    Right (LaunchTask ts) -> doLaunch ts
  
  case ret of
    Left e -> respond $ 
      Wai.responseLBS S.status404 [] $ toS ("invalid command: " ++ e)
    Right resp -> respond $ Wai.responseLBS S.status200 [] (cStrToBStr resp)

  where
    respond = liftIO . _respond
    packStr = pure . Right . C.pack
    getBody req acc = do
      body <- Wai.requestBody req
      if C.null body then return acc
                     else getBody req (C.append acc body)
    doReport = Right . toS . reportStatus <$> getState
    doAdjust a b =
      if a < 0 || b < 0
         then pure (Left "invalid command")
         else adjustResLimit a b >> pure (Right "done") -- handle exception here
    doLaunch tasks = do
      $(logTM) InfoS ("Launching " <> showLS (length tasks) <> " tasks")
      mutateState $ \st -> st { pendingTasks = (pendingTasks st) ++ tasks }
      packStr "Tasks enqueued"


launchRunner :: IO ()
launchRunner = do
  store <- newTVarIO initState
  myPath <- getExecutablePath
  let pkp = (takeDirectory myPath) </> "assets/pub.key"
  pkey <- read . bStrToString <$> B.readFile pkp
  handleScribe <- K.mkHandleScribe (K.ColorLog True) stdout K.DebugS K.V1
  let mkLogEnv =
        K.registerScribe "stdout" handleScribe K.defaultScribeSettings =<<
          K.initLogEnv "LocalRunner" "devel"
  bracket mkLogEnv K.closeScribes $ \le -> 
    Warp.run 3333 $ webApp (initConfig le store pkey)
  where
    initState = State { workerIds = []
                      , completedTasks = []
                      , pendingTasks = []
                      }
