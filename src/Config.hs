{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where


import           Data.Yaml       (FromJSON (..), (.:))
import           System.FilePath
import qualified Codec.Crypto.RSA.Pure as RSA
import qualified Data.ByteString.Char8 as B
import qualified Data.Yaml             as Y

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


data MasterConfig = MasterConfig 
  { knownHosts :: FilePath
  , credential :: Credential
  , nodes      :: [Node]
  , signPKey   :: FilePath
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

instance FromJSON MasterConfig where
  parseJSON (Y.Object v) =
    MasterConfig <$>
      v .: "known_hosts" <*>
      v .: "credential" <*>
      v .: "nodes" <*>
      v .: "private_key_forsign"
  parseJSON _ = fail "expected object for MasterConfig"


loadConfig :: FilePath -> IO MasterConfig
loadConfig path = Y.decodeFileThrow path

