module Main where

import Hangman
import Test.QuickCheck
import Test.QuickCheck.Monadic
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

instance Arbitrary Puzzle where
  arbitrary = puzzleGen

newtype LowerChar = LowerChar Char deriving Show

instance Arbitrary LowerChar where
  arbitrary = LowerChar <$> lowerGen

puzzleFill :: Puzzle -> LowerChar -> Property
puzzleFill p (LowerChar c) = monadicIO $ do
  p' <- run $ handleGuess p c
  let fillProp = if elem c (word p)
                 then elem (Just c) (discovered p')
                 else not $ elem (Just c) (discovered p')
  assert fillProp

main :: IO ()
main = do
  quickCheck puzzleFill
