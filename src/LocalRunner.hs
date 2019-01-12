{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LocalRunner where


import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Process
import System.Exit (ExitCode (..))
import Network.Wai


import Utils


data Task = Task { command  :: String
                 , args     :: [String]
                 , workDir  :: FilePath
                 , retryCnt :: Int
                 }

instance Show Task where
  show (Task cmd args _ rc) = "[" ++ (show rc) ++ "] " ++ cmd ++ (unwords args)

data MasterCommand = SetupEnv String
                   | ReportStatus
                   | AdjustResLimit Int Int
                   | RunTasks [Task]
                   deriving (Show)

data Worker = Worker Int Int

type DeviceId = Int

data State = State { workerIds :: [(DeviceId, [ThreadId])]
                   , completedTasks :: [(Task, ExitCode)]
                   , pendingTasks :: [Task]
                   }


execTask :: Task -> DeviceId -> IO ExitCode
execTask task gpuId = withCreateProcess cp $ \_ _ _ ph -> waitForProcess ph
  where
    cp = (proc (command task) (args task)) {
      cwd = Just (workDir task :: FilePath)
    , env = Just ev }
    ev = [("CUDA_VISIBLE_DEVICES", show gpuId)]


mutateSt :: TVar State -> (State -> State) -> IO ()
mutateSt storage fn = atomically $ do
  st <- readTVar storage
  writeTVar storage (fn st)


runnerThread :: TVar State -> DeviceId -> IO ()
runnerThread storage devId = loop
  where
    loop = do
      nextTask <- atomically $ do
        st <- readTVar storage
        let plist = pendingTasks st
        if null plist
          then return Nothing
          else do
            writeTVar storage (st { pendingTasks = tail plist })
            return $ Just (head plist)
      case nextTask of
         Nothing -> delayBy 500 >> loop
         Just t  -> do
           exc <- execTask t devId
           mutateSt storage $ \st -> newState st t exc
    newState st t exitCode =
      let State{completedTasks=ct, pendingTasks=pt} = st
          t' = t { retryCnt = (retryCnt t - 1) }
      in case exitCode of
              ExitFailure _ | (retryCnt t) > 0 -> st { pendingTasks = pt ++ [t'] }
              otherwise -> st { completedTasks = (t, exitCode) : ct }


adjustResLimit :: TVar State -> Int -> Int -> IO ()
adjustResLimit storage nDev nWorkerPerDev = do
  st <- atomically $ readTVar storage
  let workers = workerIds st
  workers <- let (hd, tl) = splitAt nDev workers
             in forM tl (truncateThreads 0) >> pure hd
  workers <- forM workers $ truncateThreads nWorkerPerDev
  let deltaN = nWorkerPerDev - (length (snd $ workers !! 0))
  workers <- forM workers $ \(d, l) -> do
    l' <- newThreadList d deltaN
    return (d, l ++ l')
  extraDevs <- findExtraGPUs (nDev - length workers)
  newWorkers <- forM extraDevs $ \d -> do
    nth <- newThreadList d nWorkerPerDev
    return (d, nth)
  let workers = workers ++ newWorkers
  mutateSt storage $ \st -> st { workerIds = workers }
  where
    truncateThreads nKeep (devId, tList) = do
      forM (drop nKeep tList) killThread
      return $ (devId, take nKeep tList)
    newThreadList devId nWorker = replicateM nWorker $ forkIO $
      runnerThread storage devId
    findExtraGPUs :: Int -> IO [DeviceId]
    findExtraGPUs nDev = undefined


reportStatus :: State -> String
reportStatus = undefined -- just serialize JSON?


{-
processCommand :: TVar State -> MasterCommand -> IO ()
processCommand storage = \case
  SetupEnv s -> undefined
  ReportStatus s -> reportStatus <$> (atomically $ readTVar storage)
  AdjustResLimit nDev nWorker -> adjustResLimit storage nDev nWorker
  RunTasks tasks -> atomically $ do
    st <- readTVar storage
    let pt = pendingTasks st
    writeTVar st { pendingTasks = pt ++ tasks }


runnerSupervisorThread :: TChan command -> TChan response -> IO ()
runnerSupervisorThread storage = do
  forConcurrently_ (join $ replic
  
  forever $ do
    st <- atomically $ readTVar storage
    delayBy 500
  where
    initDevIds = replicate 
-}



