{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( RemoteConfig (..), loadConfig ) where


import System.FilePath
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))

import Utils
import Lib


data RemoteConfig = RemoteConfig { knownHosts :: FilePath
                                 , credential :: Credential
                                 , nodes      :: [Node]
                                 } deriving (Show)


instance FromJSON Credential where
  parseJSON (Y.Object v) = MkCredential <$>
    v .: "public_key" <*>
    v .: "private_key"
  parseJSON _ = fail "expected object for Credential"

instance FromJSON Node where
  parseJSON (Y.Object v) = MkNode <$>
    v .: "name" <*>
    v .: "host_name" <*>
    v .: "port" <*>
    v .: "user_name"
  parseJSON _ = fail "expected object for Node"

instance FromJSON RemoteConfig where
  parseJSON (Y.Object v) =
    RemoteConfig <$>
      v .: "known_hosts" <*>
      v .: "credential" <*>
      v .: "nodes"
  parseJSON _ = fail "expected object for RemoteConfig"


loadConfig :: FilePath -> IO RemoteConfig
loadConfig path = Y.decodeFileThrow path

