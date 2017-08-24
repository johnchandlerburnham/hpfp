module Filtering where

-- ex 1
filterThreeMult = filter (\x -> x `mod` 3 /= 0)

-- ex 2
howManyThreeMults x = length x - (length . filterThreeMult) x
howManyThreeMults2 = length . filter (\x -> x `mod` 3 == 0)

-- ex 3

removeArticle :: String -> [String]
removeArticle = filter (not . isArticle) . words  
  where isArticle x = elem x ["a", "an", "the"] 

