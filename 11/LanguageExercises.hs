--11/LanguageExercises.hs
module LanguageExercises where

import Data.Char

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
  

