{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

module Task (
  MasterCommand(..), Task(..), SlaveStatus(SlaveStatus),
  TaskGroup(..), Prerequisite(..), parseCommand ) where


import           Data.Aeson
import           Data.ByteString.Lazy (ByteString, unpack)
import           Data.String.Conv     (toS)
import           Data.Text            (Text)
import           Text.Printf
import           GHC.Generics
import           System.Exit          (ExitCode (..))
import           Utils
import qualified Data.ByteString.Base64.Lazy as Base64


-- met by the server side
data Prerequisite = SyncData  FilePath FilePath -- local remote
                  | GitRepoIn FilePath FilePath String -- local remote commit
                  | RunScript FilePath
                  deriving (Generic, Show)


data TaskGroup = TaskGroup [Prerequisite] String deriving (Generic, Show)


data Task = Task
  { taskGroup :: TaskGroup
  , command   :: String
  , args      :: [String]
  , workDir   :: FilePath
  , retryCnt  :: Int
  } deriving (Generic)


instance ToJSON Prerequisite

instance FromJSON Prerequisite

instance ToJSON TaskGroup

instance FromJSON TaskGroup

instance ToJSON Task

instance FromJSON Task

instance Show Task where
  show (Task (TaskGroup _ name) cmd args _ rc) =
    printf "[Task from %s; rc %d] %s (%s)" name rc cmd (unwords args)


data MasterCommand = LaunchTask [Task]
                   | ReportStatus
                   | AdjustResLimit Int Int
                   | InitTaskGroup TaskGroup
                   deriving (Show, Generic)

instance ToJSON MasterCommand

instance FromJSON MasterCommand


data SlaveStatus = SlaveStatus
  { pendingTasks   :: [Task]
  , succeededTasks :: [Task]
  , failedTasks    :: [Task]
  , workers        :: [(Int, String)]
  } deriving (Generic)

instance ToJSON SlaveStatus

instance FromJSON SlaveStatus


parseCommand :: Show e
             => ByteString -- request body
             -> (ByteString -> ByteString -> Either e Bool) -- signature verifier
             -> Either String MasterCommand
parseCommand req verify =
  case (decode req) :: Maybe (Text, Text) of
    Nothing -> Left "decode error"
    Just (str, sig) -> 
      let str' = toS str
          sig' = Base64.decodeLenient (toS sig)
       in case verify str' sig' of
            Left e -> Left $ show e
            Right False -> Left "illegal signature"
            Right True -> case decode str' of
                            Nothing -> Left "decode failed"
                            Just ts -> Right ts

