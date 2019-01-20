{-# LANGUAGE MultiWayIf #-}

module ShUtils where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as C
import           Data.Char
import           Data.List (find)
import           Data.Maybe
import           Text.Read


data GpuInfo = GpuInfo Int String Float Float deriving (Show)


data NodeInfo = NodeInfo
  { gpuInfo    :: [GpuInfo]
  , freeDisk   :: Float
  , nCpus      :: Int
  , avgCpuUtil :: Float
  } deriving (Show)


mergeInfo :: NodeInfo -> NodeInfo -> NodeInfo
mergeInfo (NodeInfo g f n a) (NodeInfo g' f' n' a') = 
  NodeInfo g'' f'' n'' a''
  where
    upd a b = a * 0.9 + b * 0.1
    updateGpuInfo :: [GpuInfo] -> [GpuInfo] -> [GpuInfo]
    updateGpuInfo gs gs' = 
      (flip map) gs' $ \(GpuInfo i' s' fmem' utl') ->
        case find (\(GpuInfo i _ _ _) -> i == i') gs of
          Nothing -> (GpuInfo i' s' fmem' utl')
          Just (GpuInfo _ _ fmem utl) -> 
            GpuInfo i' s' (upd fmem fmem') (upd utl utl')
    g'' = updateGpuInfo g g'
    f'' = f'
    n'' = n'
    a'' = upd a a'


parseNvidiaSMI :: B.ByteString -> [GpuInfo]
parseNvidiaSMI stdout = [x | Just x <- map fromLine lines]
  where
    lines = B.lines stdout
    fromLine line = do
      let toks = map B.unpack $ B.split ',' line
      name <- pure $ toks !! 0
      id_  <- readMaybe $ toks !! 1
      fmem <- readMaybe $ toks !! 2
      util <- readMaybe $ toks !! 3
      return $ GpuInfo id_ name fmem util


nvidiaSMICmd :: String
nvidiaSMICmd = "nvidia-smi --query-gpu=name,index,memory.free,utilization.gpu \
               \--format=csv,nounits,noheader"


cpuUtilCmd :: String
cpuUtilCmd = "grep 'cpu' /proc/stat | awk '{print ($2+$4)*100/($2+$4+$5)}'"

parseCpuUtil :: String -> Either String (Int, Float)
parseCpuUtil stdout = 
  let s = map (filter (not . isSpace)) $ lines stdout
      s'= filter (not . null) s
   in if length s' <= 1 
         then Left "cpu util parse failed"
         else let gs = catMaybes (map readMaybe $ tail s') :: [Float]
               in Right (length gs, (sum gs) / (fromIntegral (length gs)))


diskUtilCmd :: String
diskUtilCmd = "df -h / | tail -n 1 | awk '{print $4}'"

parseDiskUtil :: String -> Either String Float
parseDiskUtil stdout =
  let s = filter (not . isAlpha) stdout
   in cast $ if | elem 'G' stdout -> readMaybe s
                | elem 'M' stdout -> (* 0.001) <$> readMaybe s
                | elem 'T' stdout -> (* 1e3) <$> readMaybe s
                | otherwise -> Nothing
  where
    cast Nothing = Left "prase disk util failed"
    cast (Just v) = Right v
