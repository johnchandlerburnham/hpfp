module Filtering where

--1
filterThreeMult :: [Integer] -> [Integer]
filterThreeMult = filter (\x -> x `mod` 3 /= 0)

--2
howManyThreeMults :: [Integer] -> Int
howManyThreeMults x = length x - (length . filterThreeMult) x

howManyThreeMults' :: [Integer] -> Int
howManyThreeMults' = length . filter (\x -> x `mod` 3 == 0)

--3
removeArticle :: String -> [String]
removeArticle = filter (not . isArticle) . words
  where isArticle x = elem x ["a", "an", "the"]

