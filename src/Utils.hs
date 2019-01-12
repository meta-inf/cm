module Utils (for, delayBy, assocListReplace, TimeStr, defaultTime, now) where

import Control.Monad
import Control.Concurrent
import Data.Time.LocalTime
import Data.Time.Format

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
