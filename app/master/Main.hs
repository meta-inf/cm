{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Main where

import qualified Config                 as C
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List              (sortBy)
import           Data.String.Conv       (toS)
import           Katip.Format.Time
import           Lib
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
  let nodes = sortBy (\(a,_) (b,_) -> compare a b) (clusterState st)
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
          $forall (nodeName, nodeResult) <- nodes
            $forall gpu <- enumerate (listOfRes nodeResult)
              <tr>
                $if fst gpu == 1
                  <td rowspan=#{numGPUs nodeResult}>
                    #{nodeName}
                    $case queryStatus nodeResult
                      $of Failure err
                        \ (comm error at #{queryTime nodeResult}: #{err})
                      $of _
                $case (snd gpu)
                  $of C.GpuInfo gId name fmem util
                    <td> #{gId}
                    <td> #{name}
                    <td> #{fmem}
                    <td> #{util}
                $if fst gpu == 1
                  <td rowspan=#{numGPUs nodeResult}>
                    #{lastSuccessTime nodeResult}
      <textarea rows=#{nlogs st} cols="84">
        #{renderLog st}
  |]
  where
    listOfRes r = case (lastSuccessRes r) of
                       [] -> [C.GpuInfo (-1) "undefined" 0 0]
                       _  -> lastSuccessRes r
    enumerate = zipWith (,) [1..]
    numGPUs = length . listOfRes
    nlogs st = min (4 * (length $ lastLogs st)) 100
    renderLog (MasterMutable { lastLogs = ls, .. }) =
      unlines (map showLogItem ls)
    showLogItem (t, loc, v) = toS (formatAsLogTime t) ++ " " ++ loc ++ " " ++ v
      


main :: IO ()
main = withMasterThreads "./assets/config.yaml" $
  \m -> warp 9801 App { mut = m }
