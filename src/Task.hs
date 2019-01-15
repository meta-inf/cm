{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

module Task (
  MasterCommand(..), Task(..), SlaveStatus (SlaveStatus), parseCommand ) where


import           Data.Aeson
import           Data.ByteString.Lazy (ByteString, unpack)
import           Data.String.Conv     (toS)
import           Data.Text            (Text)
import           GHC.Generics
import           System.Exit          (ExitCode (..))
import           Utils
import qualified Data.ByteString.Base64.Lazy as Base64


data Task = Task
  { command  :: String
  , args     :: [String]
  , workDir  :: FilePath
  , retryCnt :: Int
  } deriving (Generic)

instance ToJSON Task

instance FromJSON Task

instance Show Task where
  show (Task cmd args _ rc) = "[" ++ (show rc) ++ "] " ++ cmd ++ (unwords args)


data MasterCommand = LaunchTask [Task]
                   | ReportStatus
                   | AdjustResLimit Int Int
                   deriving (Show, Generic)

instance ToJSON MasterCommand

instance FromJSON MasterCommand


data SlaveStatus = SlaveStatus
  { pendingTasks :: [Task]
  , succeededTasks :: [Task]
  , failedTasks :: [Task]
  , workers :: [(Int, String)]
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

