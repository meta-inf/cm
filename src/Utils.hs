module Utils where

import Control.Monad
import Control.Concurrent
import Data.Time.LocalTime
import Data.Time.Format
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Internal  as BS (c2w, w2c)

for :: Monad m => m a -> (a -> b) -> m b
for = flip fmap

delayBy :: Int -> IO ()
delayBy millisecs = threadDelay (1000 * millisecs)

assocListReplace :: Eq k => k -> a -> [(k, a)] -> [(k, a)]
assocListReplace k v lst = (k, v) : (filter ((k /=) . fst) lst)

type TimeStr = String

defaultTime = "n/a"

now :: IO TimeStr
now = do
  s <- getZonedTime
  return ((formatTime defaultTimeLocale rfc822DateFormat s) :: TimeStr)

toLazyByteString :: C.ByteString -> B.ByteString
toLazyByteString = B.pack . map BS.c2w . C.unpack
