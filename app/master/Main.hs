{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Main where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List              (sortBy)
import           Text.Hamlet            (hamletFile)
import           Utils
import           Yesod

import qualified Config                 as C
import qualified Lib                    as L


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
                  $of C.GpuInfo gId name fmem util
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
                       [] -> [C.GpuInfo (-1) "undefined" 0 0]
                       _  -> L.lastSuccessRes r
    enumerate = zipWith (,) [1..]
    numGPUs = length . listOfRes


main :: IO ()
main = do
  qstate <- L.launchMasterThreads "./assets/config.yaml"
  warp 9801 App { .. }

