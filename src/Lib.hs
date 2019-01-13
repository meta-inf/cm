module Lib where


import qualified Data.ByteString.Char8 as B
import Text.Read
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Network.SSH.Client.SimpleSSH
import Data.Maybe (fromJust)

import App
import Utils
import Config


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


queryNode :: Node -> MasterApp (Either String [GpuInfo])
queryNode nd = do
  crd <- asksApp credential
  let app = 
        (withSessionKey (hostName nd) (port nd) knownHosts (userName nd)
         (publicKey crd) (privateKey crd) "") (\s -> execCommand s queryCmd)
  liftIO (runSimpleSSH app) `for` \case
    Left err     -> returnErr $ "ssh connection failed: " ++ show err
    Right result -> 
      case resultExit result of
           ExitSuccess -> Right $ parseNvidiaSMI $ resultOut result
           _           -> returnErr $ "nvidia-smi failed: " ++ show result
  where
    returnErr s = Left s
    queryCmd = "nvidia-smi --query-gpu=name,index,memory.free,utilization.gpu \
      \--format=csv,nounits,noheader"
    knownHosts = "/home/if/.ssh/known_hosts"


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


launchMasterThreads :: FilePath -> IO (TVar ClusterState)
launchMasterThreads cpath = do
  cfg <- loadConfig cpath
  storage <- newTVarIO (initState (nodes cfg))
  let s = initConfig undefined storage cfg 
  forkIO $ runApp s $ queryThread 500
  return storage

