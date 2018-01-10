--11/AsPatterns.hs
module AsPatterns where

import Data.Char

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

-- 1
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) b = elem x b && isSubsequenceOf xs b

-- 2
capWords :: String -> [(String, String)]
capWords str = map cap $ words str where
  cap a@(x:xs) = (,) a $ (toUpper x):xs
