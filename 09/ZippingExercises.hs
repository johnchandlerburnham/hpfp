-- 09/ZippingExercises.hs
module ZippingExercises where

myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys
myZip _ _ = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)
myZipWith _ _ _ = []
