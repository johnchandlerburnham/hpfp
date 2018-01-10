--10/FunctionsUsingFolds.hs
module FunctionsUsingFolds where

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


