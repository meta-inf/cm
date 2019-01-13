module Main where


import Codec.Crypto.RSA.Pure
import Crypto.Random
import System.Random


main :: IO ()
main = do
  g <- newGenIO :: IO SystemRandom
  case generateKeyPair g 0x1001 of
    Left e -> putStrLn $ "generation error: " ++ show e
    Right (pub, priv, g') -> do
      writeFile "./assets/pub.key" (show pub)
      writeFile "./assets/priv.key" (show priv)
