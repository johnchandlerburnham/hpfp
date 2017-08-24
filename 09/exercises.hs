module Exercises where

import Data.Char

-- 1 :t isUpper

-- 2 

filterUpper = filter isUpper

-- 3 

capFirst :: String -> String
capFirst "" = "" 
capFirst (x:xs) = (toUpper x):xs

-- 4

strToUpper :: String -> String
strToUpper "" = "" 
strToUpper (x:xs) = (toUpper x):(strToUpper xs)

-- 5 

headToUpper :: String -> Char
headToUpper = toUpper . head

-- 6, wrote it pointfree the first time...



