module ZippingExercises where

--1
myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys
myZip _ _ = []

--2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)
myZipWith _ _ _ = []

--3
myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)
