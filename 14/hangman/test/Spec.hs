module Spec where

import Hangman
import Test.QuickCheck
import Control.Monad (guard)
import Data.List (nub)
import Data.Char (isAsciiLower)


lowerGen :: Gen Char
lowerGen = suchThat arbitrary (isAsciiLower)

wordGen :: Gen String
wordGen = resize 9 $ listOf lowerGen

puzzleGen :: Gen Puzzle
puzzleGen = do
  word <- wordGen
  guesses <- wordGen
  return $ Puzzle word guesses

main :: IO ()
main = do
  return ()
