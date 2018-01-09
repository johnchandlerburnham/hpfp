module CharExercises where

import Data.Char

--1
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char

-- 2 

filterUpper = filter isUpper

-- 3 

capFirst :: String -> String
capFirst (x:xs) = (toUpper x):xs
capFirst _ = "" 

-- 4

strToUpper :: String -> String
strToUpper (x:xs) = (toUpper x):(strToUpper xs)
strToUpper _ = "" 

-- 5 

headToUpper :: String -> Char
headToUpper = toUpper . head

-- 6, wrote it pointfree the first time...



