{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE MultiWayIf      #-}

module Main where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List              (sortBy)
import           Data.String.Conv       (toS)
import           Katip.Format.Time
import           Lib
import           ShUtils                (GpuInfo (..), NodeInfo (..))
import           Text.Hamlet            (hamletFile)
import           Utils
import           Yesod


data App = App { mut :: TVar MasterMutable }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
  defaultLayout contents = do
    PageContent title headTags bodyTags <- widgetToPageContent contents
    withUrlRenderer $(hamletFile "html-templates/default-layout.hamlet")


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  App { .. } <- getYesod
  st <- liftIO $ atomically $ readTVar mut
  setTitle "Home"
  let nodes = sortBy (\(_,a) (_,b) -> compareNode b a) (clusterState st)
  [whamlet|
      <table class="ui celled unstackable structured table" style="width:80%">
        <thead>
          <tr>
            <th> Node
            <th> GPUs
            <th> Free Disk (GB)
            <th> CPUs (Avg Util)
            <th> Update Time
        <tbody>
          $forall (nodeName, nodeQueryResult) <- nodes
            <tr>
              <td>
                #{nodeName}
                $case queryStatus nodeQueryResult
                  $of Failure err
                    \ (comm. error at #{queryTime nodeQueryResult}: #{err})
                  $of _
              <td>
                $forall gpu <- enumerate (gpuInfo $ lastSuccessRes nodeQueryResult)
                  $case (snd gpu)
                    $of GpuInfo gId name fmem util
                      <li style="color:#{gpuColor (snd gpu)}">
                        #{gId}; #{name}; #{fmem} MB; #{util} %
              <td style="color:#{diskColor $ lastSuccessRes nodeQueryResult}">
                #{freeDisk $ lastSuccessRes nodeQueryResult}
              <td style="color:#{cpuColor $ lastSuccessRes nodeQueryResult}">
                #{nCpus $ lastSuccessRes nodeQueryResult}
                (#{avgCpuUtil $ lastSuccessRes nodeQueryResult})
              <td>
                #{lastSuccessTime nodeQueryResult}
      <textarea rows=#{nlogs st} cols="84">
        #{renderLog st}
  |]
  where
    enumerate = zipWith (,) [1..]
    nlogs st = min (4 * (length $ lastLogs st)) 100
    renderLog (MasterMutable { lastLogs = ls, .. }) =
      unlines (map showLogItem ls)
    showLogItem (t, loc, v) =
      toS (formatAsLogTime t) ++ " " ++ loc ++ " " ++ v
    compareNode n1 n2 =
      compare (nFreeGPUs $ lastSuccessRes n1) (nFreeGPUs $ lastSuccessRes n2)
    nFreeGPUs = map (\g -> gpuColor g == green) . gpuInfo
    green  = "#11aa11" :: String
    yellow = "#bbbb11" :: String
    red    = "#ff1111" :: String
    cpuColor (NodeInfo { nCpus = nc, avgCpuUtil = ac }) = 
      if | ac > 90 -> red
         | ac > 50 -> yellow
         | otherwise -> green
    diskColor (NodeInfo { freeDisk = fd }) =
      if | fd < 8 -> red
         | fd < 20 -> yellow
         | otherwise -> green
    gpuColor (GpuInfo _ _ fmem utl) =
      if | fmem < 800 || utl > 90 -> red
         | fmem < 2000 || utl > 50 -> yellow
         | otherwise -> green


main :: IO ()
main = withMasterThreads "./assets/config.yaml" $
  \m -> warp 9801 App { mut = m }
