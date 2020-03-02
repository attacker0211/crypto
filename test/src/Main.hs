module Main where

import           CodeGen.SuccessiveSquaring
import           CodeGen.ExtendedGCD

main :: IO ()
main = do
  putStrLn "Enter a, b"
  a <- readLn
  b <- readLn
  putStrLn (show (genEucl a b))
  putStrLn (show (genEGCDT a b))
  --putStrLn "Enter g, h, p"
  --g <- readLn
  --h <- readLn
  --p <- readLn
  --putStrLn (show (genBase h))
  --putStrLn (show (genSuccessive g h p))
  --putStrLn (show (genRes g h p))

