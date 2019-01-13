{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module LocalRunner where


import Control.Exception      (bracket)
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import System.Process
import System.Exit            (ExitCode(..))
import System.IO              (stdout)
import Data.List              (sortBy)
import Text.Read              (readMaybe)
import Text.JSON
import Katip                  (logTM, Severity(..), showLS, ls)
import qualified Data.Monoid
import qualified Codec.Crypto.RSA.Pure     as RSA
import qualified Data.Text                 as Text
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Lazy      as B
import qualified Network.Wai               as Wai
import qualified Network.Wai.Handler.Warp  as Warp
import qualified Network.HTTP.Types.Status as S
import qualified Katip                     as K

import Lib (parseNvidiaSMI, GpuInfo (..))
import Utils
import Task


type DeviceId = Int
type WorkerId = Int

data State = State { workerIds :: [(DeviceId, [(WorkerId, ThreadId)])]
                   , completedTasks :: [(Task, ExitCode)]
                   , pendingTasks :: [Task]
                   }

data Config = Config { storage :: TVar State
                     , logNamespace :: K.Namespace
                     , logContext :: K.LogContexts
                     , logEnv :: K.LogEnv
                     , publicKey :: RSA.PublicKey
                     }

newtype LocalApp a = LocalApp { unLocalApp :: ReaderT Config IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

instance K.Katip LocalApp where
  getLogEnv = asks logEnv
  localLogEnv f (LocalApp m) =
    LocalApp (local (\s -> s { logEnv = f (logEnv s)}) m)

instance K.KatipContext LocalApp where
  getKatipContext = asks logContext
  localKatipContext f (LocalApp m) =
    LocalApp (local (\s -> s { logContext = f (logContext s)}) m)
  getKatipNamespace = asks logNamespace
  localKatipNamespace f (LocalApp m) =
    LocalApp (local (\s -> s { logNamespace = f (logNamespace s)}) m)


runLocalApp :: Config -> LocalApp a -> IO a
runLocalApp c k = runReaderT (unLocalApp k) c


toIOAction :: LocalApp a -> LocalApp (IO a)
toIOAction k = ask `for` \c -> runLocalApp c k


liftSTM :: STM a -> LocalApp a
liftSTM = liftIO . atomically


getState :: LocalApp State
getState = do
  storage <- asks storage
  liftSTM $ readTVar storage


mutateSt :: (State -> State) -> LocalApp ()
mutateSt fn = do
  storage <- asks storage
  liftSTM $ do
    st <- readTVar storage
    writeTVar storage (fn st)


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
      storage <- asks storage
      nextTask <- liftSTM $ do
        st <- readTVar storage
        let plist = pendingTasks st
        if null plist
          then return Nothing
          else do
            writeTVar storage (st { pendingTasks = tail plist })
            return $ Just (head plist)
      -- launch
      case nextTask of
        Nothing -> liftIO (delayBy 1000) >> loop
        Just t  -> do
          $(logTM) InfoS ("Launching task " <> showLS t)
          exc <- execTask t devId
          $(logTM) InfoS ("Task finished, exit code " <> showLS exc)
          mutateSt $ \st -> newState st t exc
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
  mutateSt $ \st -> st { workerIds = workers }
  $(logTM) InfoS ("resource limit adjusted to " <> showLS (nDev, nWorkerPerDev))

  where
    truncateThreads nKeep (devId, tList) = do
      forM (drop nKeep tList) (liftIO . killThread . snd)
      return (devId, take nKeep tList)
    newThreadList devId workerIdRange = forM workerIdRange $ \wId -> do
      action <- toIOAction $ runnerThread devId wId
      tId <- liftIO (forkIO action)
      return (wId, tId)


reportStatus :: State -> String
reportStatus st = showJSObject jval ""
  where
    jval = toJSObject [
      ("pending_tasks", showJSON $ length (pendingTasks st)),
      ("completed_tasks", showJSON $ length (completedTasks st)),
      ("workers", JSArray (map (showJSON . print_) $ workerIds st))]
    print_ (a, ls) = (a, map (\(x,y)->(x, show y)) ls)
--  wlist = map (\(i, lst) -> (i::Int, length lst)) (workerIds st)


webApp :: Config -> Wai.Application
webApp cfg req _respond = runLocalApp cfg $ do
  reqBody <- liftIO $ getBody req C.empty
  let reqPath = map Text.unpack (Wai.pathInfo req)
  $(logTM) InfoS ("request: " <> showLS reqPath <> K.ls reqBody)
  responseText <- case reqPath of
                    ["report"]      -> doReport
                    "adjust":a:b:[] -> doAdjust a b
                    ["launch"]      -> doLaunch $ cStrToBStr reqBody
                    _               -> pure Nothing
  case responseText of
    Nothing -> respond $ Wai.responseLBS S.status404 [] "invalid command"
    Just t  -> respond $ Wai.responseLBS S.status200 [] (cStrToBStr t)

  where

    respond = liftIO . _respond
    packStr = pure . Just . C.pack

    getBody req acc = do
      body <- Wai.requestBody req
      if C.null body then return acc
                     else getBody req (C.append acc body)

    doReport = Just . C.pack . reportStatus <$> getState

    doAdjust argA argB = 
      case (readMaybe argA, readMaybe argB) of
        (Just a, Just b) | a > 0 &&  b > 0 ->
          adjustResLimit a b >> pure (Just "done") -- handle exception here
        _ -> pure Nothing

    doLaunch reqBody =
      do
        verify <- asks publicKey `for` RSA.verify
        case parseTaskGroup reqBody verify of
          Left err -> packStr $ "Error parsing request body: " ++ err
          Right ts -> do
            $(logTM) InfoS ("Launching " <> showLS (length ts) <> " tasks")
            mutateSt $ \st -> st { pendingTasks = (pendingTasks st) ++ ts }
            packStr "Tasks enqueued"


launchRunner :: IO ()
launchRunner = do
  store <- newTVarIO initState
  pkey <- read . bStrToString <$> B.readFile "./assets/pub.key"
  handleScribe <- K.mkHandleScribe (K.ColorLog True) stdout K.DebugS K.V1
  let mkLogEnv =
        K.registerScribe "stdout" handleScribe K.defaultScribeSettings =<<
          K.initLogEnv "LocalRunner" "devel"
  bracket mkLogEnv K.closeScribes $ \le -> do
    let cfg = Config { logEnv = le
                     , logContext = mempty
                     , logNamespace = Data.Monoid.mempty
                     , storage = store
                     , publicKey = pkey
                     }
    Warp.run 3333 $ webApp cfg
  where
    initState = State { workerIds = []
                      , completedTasks = []
                      , pendingTasks = []
                      }

