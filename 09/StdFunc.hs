--09/StdFunc.hs
module StdFunc where

-- 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = (myOr . map f) xs

-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem x xs = myAny ((==) x) xs

-- 4
myReverse :: [a] -> [a]
myReverse xs = go xs []
  where go [] n = n
        go (n:ns) a = go ns (n:a)

-- 5
squish :: [[a]] -> [a]
squish [] = []
squish ((n:[]):nss) = n : squish (nss)
squish ((n:ns):nss) = n : squish (ns:nss)

-- 6 
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = go f xs []
  where go _ [] [] = []
        go f (x:xs) [] = go f xs (f x)
        go f xs (a:as) = a : (go f xs as)

-- 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8 
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp (x:xs) = go cmp xs x
  where go _ [] a = a
        go cmp (x:xs) a = go cmp xs (if (cmp x a) == GT then x else a)

-- 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp (x:xs) = go cmp xs x
  where go _ [] a = a
        go cmp (x:xs) a = go cmp xs (if (cmp x a) == LT then x else a)


myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare



 
