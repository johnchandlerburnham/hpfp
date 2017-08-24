module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord x = 
  case x of 
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    0 -> "zero"
    _ -> "NaD"

digits :: Int -> [Int]
digits n = go n []
  where go n d
          | n < 10 = n:d
          | n >= 10 = go (n `div` 10) $ (n `mod` 10):d
          
wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
