module ZipEx where

myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys
myZip [] _ = []
myZip _ [] = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)
myZipWith _ [] _ = []
myZipWith _ _ [] = []
