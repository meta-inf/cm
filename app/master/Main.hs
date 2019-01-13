{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import Control.Monad
import Control.Concurrent     (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.List              (sortBy)
import Yesod
import Text.Hamlet            (hamletFile)
import Utils
import qualified Lib as L
import qualified Config as C


data App = App
         { qstate :: TVar L.ClusterState
         }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
  defaultLayout contents = do
    PageContent title headTags bodyTags <- widgetToPageContent contents
    withUrlRenderer $(hamletFile "templates/default-layout.hamlet")


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  App {..} <- getYesod
  clusterState <- liftIO $ atomically $ readTVar qstate
  setTitle "Home"
  let qstate' = sortBy (\(a,_) (b,_) -> compare a b) clusterState
  [whamlet|
      <table class="ui celled unstackable structured table" style="width:50%">
        <thead>
          <tr>
            <th> Node
            <th> Gpu Id
            <th> Name
            <th> Free Mem
            <th> Util.
            <th> Update Time
        <tbody>
          $forall (nodeName, nodeResult) <- qstate'
            $forall gpu <- enumerate (listOfRes nodeResult)
              <tr>
                $if fst gpu == 1
                  <td rowspan=#{numGPUs nodeResult}>
                    #{nodeName}
                    $case L.queryStatus nodeResult
                      $of L.Failure err
                        \ (comm error at #{L.queryTime nodeResult}: #{err})
                      $of _
                $case (snd gpu)
                  $of L.GpuInfo gId name fmem util
                    <td> #{gId}
                    <td> #{name}
                    <td> #{fmem}
                    <td> #{util}
                $if fst gpu == 1
                  <td rowspan=#{numGPUs nodeResult}>
                    #{L.lastSuccessTime nodeResult}
  |]
  where
    listOfRes r = case (L.lastSuccessRes r) of
                       [] -> [L.GpuInfo (-1) "undefined" 0 0]
                       _  -> L.lastSuccessRes r
    enumerate = zipWith (,) [1..]
    numGPUs = length . listOfRes


main :: IO ()
main = do
  cfg <- C.loadConfig "./assets/config.yaml"
  qstate <- newTVarIO (L.initState (C.nodes cfg))
  forkIO $ L.queryThread 500 (C.nodes cfg) (C.credential cfg) qstate
  warp 9801 App { .. }

