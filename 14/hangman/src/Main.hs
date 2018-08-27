module Main where

import Hangman
import Data.Char (toLower)
import System.Random (randomRIO)

main :: IO ()
main = do
  word <- gameWord (5, 9) "data/dict.txt"
  putStrLn "Welcome to Hangman!"
  runGame $ newPuzzle $ toLower <$> word

gameWord :: (Int, Int) -> String -> IO String
gameWord (min, max) file = do
  let filterWords ws = (filter (\w -> min <= (length w) && (length w) < max) ws)
  dictEntries <- filterWords <$> lines <$> readFile file
  randomIndex <- randomRIO (0, (length dictEntries) - 1)
  return $ dictEntries !! randomIndex


