{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( GpuInfo (..), Node (..), Credential (..), QueryStatus (..),
    QueryResult (..), ClusterState, queryThread, initState
  ) where


import qualified Data.ByteString.Char8 as B
import Text.Read
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Network.SSH.Client.SimpleSSH
import Data.Maybe (fromJust)

import Utils


data GpuInfo = GpuInfo Int String Float Float deriving (Show)

data Node = MkNode { nodeName :: String
                   , hostName :: String
                   , port :: Integer
                   , userName :: String
                   } deriving (Show)

data Credential = MkCredential { publicKey :: String
                               , privateKey :: String
                               } deriving (Show)

data QueryStatus = Success | Failure String deriving (Show)

data QueryResult = QueryResult { queryStatus     :: QueryStatus
                               , queryTime       :: TimeStr
                               , lastSuccessRes  :: [GpuInfo]
                               , lastSuccessTime :: TimeStr
                               } deriving (Show)

type ClusterState = [(String, QueryResult)]


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


queryNode :: Credential -> Node -> IO (Either String [GpuInfo])
queryNode crd nd = runSimpleSSH app `for` \case
  Left err     -> returnErr $ "ssh connection failed: " ++ show err
  Right result -> 
    case resultExit result of
         ExitSuccess -> Right $ parseNvidiaSMI $ resultOut result
         _           -> returnErr $ "nvidia-smi failed: " ++ show result
  where
    app = (withSessionKey (hostName nd) (port nd) knownHosts (userName nd)
          (publicKey crd) (privateKey crd) "") (\s -> execCommand s queryCmd)
    returnErr s = Left s
    queryCmd = "nvidia-smi --query-gpu=name,index,memory.free,utilization.gpu \
      \--format=csv,nounits,noheader"
    knownHosts = "/home/if/.ssh/known_hosts"


initState :: [Node] -> ClusterState
initState = map $ \nd -> (nodeName nd, def)
  where def = QueryResult (Failure "Empty") defaultTime [] defaultTime


queryThread :: Int -> [Node] -> Credential -> TVar ClusterState -> IO ()
queryThread delay nodeList cred tvar = do
  lastResultList <- atomically $ readTVar tvar
  let maintainSingleNode nd = forever $ do
        let nId = nodeName nd
            (rTime, rVal) = infoFrom . fromJust $ lookup nId lastResultList
        newRes <- queryNode cred nd
        cTime <- now
        let newRes' =
              case newRes of
                   Left err  -> QueryResult (Failure err) cTime rVal rTime
                   Right res -> QueryResult Success       cTime res  cTime
        atomically $ do
          curList <- readTVar tvar
          writeTVar tvar $ assocListReplace nId newRes' curList
        delayBy delay
  mapConcurrently_ maintainSingleNode nodeList
  where
    infoFrom res = (lastSuccessTime res, lastSuccessRes res)


