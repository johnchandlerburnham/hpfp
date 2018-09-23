module Main where

import qualified Index1 as I1
import qualified Index2 as I2
import qualified Index3 as I3
import qualified Index4 as I4

main :: IO ()
main = do
  putStrLn "Benchmark `!?` 1"
  I1.main
  putStrLn "Benchmark `!?` 2"
  I2.main
  putStrLn "Benchmark `!?` 3"
  I3.main
  putStrLn "Benchmark `!?` 4"
  I4.main

