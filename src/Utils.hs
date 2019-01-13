module Utils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Time.LocalTime
import Data.Time.Format
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Char8     as C
import qualified Data.ByteString.Internal  as BS (c2w, w2c)

for :: Monad m => m a -> (a -> b) -> m b
for = flip fmap

delayBy :: MonadIO m => Int -> m ()
delayBy millisecs = liftIO $ threadDelay (1000 * millisecs)

assocListReplace :: Eq k => k -> a -> [(k, a)] -> [(k, a)]
assocListReplace k v lst = (k, v) : (filter ((k /=) . fst) lst)

type TimeStr = String

defaultTime = "n/a"

now :: MonadIO m => m TimeStr
now = do
  s <- liftIO getZonedTime
  return ((formatTime defaultTimeLocale rfc822DateFormat s) :: TimeStr)

cStrToBStr :: C.ByteString -> B.ByteString
cStrToBStr = B.pack . map BS.c2w . C.unpack

bStrToString :: B.ByteString -> String
bStrToString = map BS.w2c . B.unpack
