module Main where

import           CodeGen.SuccessiveSquaring
import           Algorithm.SuccessiveSquaring

main :: IO ()
main = do
  putStrLn "Enter g, h, p"
  g <- readLn
  h <- readLn
  p <- readLn
  putStrLn (show (genBase h))
  putStrLn (show (genSuccessive g h p))
  putStrLn (show (genRes g h p))
