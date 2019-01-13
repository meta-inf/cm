{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Task ( Task(..), parseTaskGroup ) where


import Text.JSON
import Data.ByteString.Lazy (ByteString, unpack)

import Utils


data Task = Task { command  :: String
                 , args     :: [String]
                 , workDir  :: FilePath
                 , retryCnt :: Int
                 }


instance Show Task where
  show (Task cmd args _ rc) = "[" ++ (show rc) ++ "] " ++ cmd ++ (unwords args)


get :: [(String, a)] -> String -> Result a
get arr k = case lookup k arr of
              Nothing -> Error ("Cannot find key " ++ k)
              Just val -> Ok val


instance JSON Task where
  readJSON (JSObject jv) =
    let mp = fromJSObject jv
     in do
       a <- get mp "command" >>= readJSON :: Result JSString
       args <- get mp "args" >>= readJSON :: Result [JSString]
       workDir <- get mp "work_dir" >>= readJSON :: Result JSString
       return $ Task { command = fromJSString a
                     , args = map fromJSString args
                     , workDir = (fromJSString workDir) :: FilePath
                     , retryCnt = 2 -- TODO magic number
                     }
  readJSON _ = Error "need a JSObject for Task"
  showJSON t = JSObject $ toJSObject [
    ("command", showJSON $ command t),
    ("args", showJSON $ args t),
    ("work_dir", showJSON (workDir t :: String))]


parseTaskGroup :: Show e
               => ByteString -- request body
               -> (ByteString -> ByteString -> Either e Bool) -- signature verifier
               -> Either String [Task]
parseTaskGroup req verify =
  case decode (bStrToString req) :: Result [ByteString] of
    Error e -> Left e
    Ok val | length val /= 2 -> Left "expecting list of 2 strings in request"
    Ok [str, sig] -> case verify str sig of
                       Left e -> Left $ show e
                       Right False -> Left "illegal signature"
                       Right True -> resultToEither $ decode (bStrToString str)

