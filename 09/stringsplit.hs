module StringSplit where

split :: String -> [String]

split x
  | x == [] = []
  | otherwise = word : split rest
    where word = takeWhile (/= ' ') x
          rest = (drop 1) $ dropWhile (/= ' ') x

split2 [] = []  
split2 x = word : split rest
    where word = takeWhile (/= ' ') x
          rest = (drop 1) $ dropWhile (/= ' ') x

splitOn :: String -> Char -> [String]
splitOn x y
  | x == [] = []
  | otherwise = word : split rest
    where word = takeWhile (/= y) x
          rest = (drop 1) $ dropWhile (/= y) x
