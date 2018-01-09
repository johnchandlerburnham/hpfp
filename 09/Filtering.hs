-- 09/Filtering.hs
module Filtering where

filterThreeMult :: [Integer] -> [Integer]
filterThreeMult = filter (\x -> x `mod` 3 /= 0)

howManyThreeMults :: [Integer] -> Int
howManyThreeMults x = length x - (length . filterThreeMult) x

howManyThreeMults' :: [Integer] -> Int
howManyThreeMults' = length . filter (\x -> x `mod` 3 == 0)

removeArticle :: String -> [String]
removeArticle = filter (not . isArticle) . words  
  where isArticle x = elem x ["a", "an", "the"] 

