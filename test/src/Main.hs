module Main where

import           CodeGen.SuccessiveSquaring
import           CodeGen.ExtendedGCD
import           Algorithm.ExtendedGCD

main :: IO ()
main = do
  putStrLn "Enter a, b"
  a <- readLn
  b <- readLn
  putStrLn (show (genEucl a b))
  putStrLn (show (divList a b))
  putStrLn (show (tableF (divList a b)))
  putStrLn (show (tableS (divList a b)))
  putStrLn (show (genEGCDT a b))
  --putStrLn "Enter g, h, p"
  --g <- readLn
  --h <- readLn
  --p <- readLn
  --putStrLn (show (genBase h))
  --putStrLn (show (genSuccessive g h p))
  --putStrLn (show (genRes g h p))

