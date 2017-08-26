module Main where

import Hello
import DogsRule
import System.IO

main :: IO ()
main = do
  putStr "Name?"
  name <- getLine
  sayHello name
  dogs
