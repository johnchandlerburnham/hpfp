module Exercises where

-- Warm-up and review
-- 1

stops = "pbtdkg"
vowels = "aeiou"

-- 1a
stopVowelStop = [(a, b, c) | a <- stops, b <- vowels, c <- stops]

-- 1b

pVowelStop = [('p', b, c) | b <- vowels, c <- stops]

-- 1c

nouns = ["cat", "dog", "ball", "box"]
verbs = ["throws", "catches", "jumps", "fetches"]

nounVerbNoun =   [(a, b, c) | a <- nouns, b <- verbs, c <- nouns]

-- 2

seekritFunc x = div (sum (map length (words x))) (length (words x))

-- function is average word length
avgWordLength :: String -> Int
avgWordLength x = div totalWordLengths numberOfWords where
  wordList = words x
  numberOfWords = length wordList 
  wordLengths = map length wordList
  totalWordLengths = sum wordLengths

-- 3
preciseAvgWordLength :: String -> Double
preciseAvgWordLength x = totalWordLength / numberOfWords
  where totalWordLength = fromIntegral $ sum $ map length $ words x
        numberOfWords   = fromIntegral $ length $ words x

-- rewriting functions using folds

-- Prime number machine

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

-- 1 
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f x = foldl check False x where
  check x y = x || f y 

-- 3 

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = myAny ((==) x) xs

-- 4

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr ((:) . f) [] xs

-- 6 

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs =  foldr g [] xs where
  g x y = if (f x) then (x : y) else y

-- 7 
squish :: [[a]] -> [a] 
squish = foldr (++) []

-- 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id 

-- 10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy ord xs = foldr1 g xs
  where g x y = if (ord x y) == GT then x else y

-- 11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy ord xs = foldr1 g xs
  where g x y = if (ord x y) == LT then x else y



