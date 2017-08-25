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

-- Language exercises
capWord :: String -> String
capWord a@(x:xs) = if x >= 'a' && x <= 'z' 
                          then chr((ord x) - 32):xs 
                          else a

capParagraph :: String -> String
capParagraph p = init $ concat $ map (++ " ") $ capp $ words p where
  capp  [] = []
  capp  (x:xs) = if (last x) == '.' then x:(capp' xs) else x:(capp xs)
  capp' [] = []
  capp' (x:xs) = if (last x) == '.' then (capWord x):(capp' xs) else x:(capp xs)
  

