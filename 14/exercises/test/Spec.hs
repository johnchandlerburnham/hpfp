module Main where

import qualified WordNumberTest as WN
import qualified Exercises as EX
import Data.Char (toUpper)
import Data.List (sort)
import Test.QuickCheck

-- 1
halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . EX.half

prop_twiceHalf :: (Eq a, Fractional a) => a -> Bool
prop_twiceHalf n = (halfIdentity n) == n

--2
listOrdered ::  (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs where
  go _ status@(_ , False) = status
  go y (Nothing, t) = (Just y, t)
  go y (Just x, t) = (Just y, x >= y)

prop_sort :: (Ord a) => [a] -> Bool
prop_sort = listOrdered . sort

-- 3
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

-- 4
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x + (y + z) == (x + y) + z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x + y == y + x

-- 5
quotRemRule :: Integral a => a -> a -> Bool
-- quotRemRule _ 0 = True
quotRemRule x y = (quot x y) * y + (rem x y) == x

divModRule :: Integral a => a -> a -> Bool
-- divModRule _ 0 = True
divModRule x y = (div x y) * y + (mod x y) == x

-- 6
expAssociative :: (Num a, Integral a, Eq a) => a -> a -> a -> Bool
expAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

expCommutative :: (Num a, Integral a, Eq a) => a -> a -> Bool
expCommutative x y = x ^ y == y ^ x

-- 7
listReverse :: (Eq a) => [a] -> Bool
listReverse list = (reverse . reverse) list == list

-- 8
prop_apply :: (Eq b) => (a -> b) -> a -> Bool
prop_apply f x = (f $ x) == (f x)

prop_compose :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_compose f g x = (f . g) x == f (g x)

instance Show (a -> b) where show _ = "Arbitrary Function"

-- 9
prop_compareCons :: (Eq a) => [a] -> [a] -> Bool
prop_compareCons xs ys = foldr (:) xs ys == (++) xs ys

prop_compareCons' :: (Eq a) => [a] -> [a] -> Bool
prop_compareCons' xs ys = foldr (:) xs ys == (flip (++)) xs ys

prop_compareConcat :: (Eq a, Foldable t) => t [a] -> Bool
prop_compareConcat xs = foldr (++) [] xs == concat xs

-- 10
prop_take :: Int -> [b] -> Bool
prop_take n xs = length (take n xs) == n

-- 11
prop_read :: (Eq a, Show a, Read a) => a -> Bool
prop_read x = (read (show x)) == x

-- Idempotence
twice f = f . f
fourTimes = twice . twice

idem1 ::  String -> Bool
idem1 x = (capw x == twice capw x) && (capw x == fourTimes capw x) where
  capw (x:xs) = (toUpper x) : xs
  capw [] = []

idem2 :: (Eq a, Ord a) => [a] -> Bool
idem2 x = (sort x == twice sort x) && (sort x == fourTimes sort x)

-- Make a Gen
data Fool = Fulse | Frue deriving (Eq, Show)

instance Arbitrary Fool where
  arbitrary = frequency [(1, return Fulse), (1, return Frue)]

foolGen :: Gen Fool
foolGen = arbitrary

fulsishFoolGen :: Gen Fool
fulsishFoolGen = do
  a <- arbitrary
  frequency [(2, return a), (1, return a)]

main :: IO ()
main = do
  WN.main
  putStrLn "prop_twiceHalf: "
  quickCheck (prop_twiceHalf :: Double -> Bool)
  putStrLn "prop_sort: "
  quickCheck (prop_sort :: [Int] -> Bool)
  putStrLn "plusAssociative: "
  quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
  putStrLn "plusCommutative: "
  quickCheck (plusCommutative :: Int -> Int -> Bool)
  putStrLn "multAssociative: "
  quickCheck (multAssociative :: Int -> Int -> Int -> Bool)
  putStrLn "multCommutative: "
  quickCheck (multCommutative :: Int -> Int -> Bool)
  putStrLn "quotRemRule:"
  quickCheck (quotRemRule :: Int -> Int -> Bool)
  putStrLn "divModRule:"
  quickCheck (divModRule :: Int -> Int -> Bool)
  putStrLn "expAssociative: "
  quickCheck (expAssociative :: Int -> Int -> Int -> Bool)
  putStrLn "expCommutative: "
  quickCheck (expCommutative :: Int -> Int -> Bool)
  putStrLn "listReverse: "
  quickCheck (listReverse :: [Char] -> Bool)
  putStrLn "prop_apply:"
  quickCheck (prop_apply :: (Int -> Int) -> Int -> Bool)
  putStrLn "prop_compose:"
  quickCheck (prop_compose :: (Int -> Int) -> (Int -> Int) -> Int -> Bool)
  putStrLn "prop_composeCons:"
  quickCheck (prop_compareCons :: String -> String -> Bool)
  putStrLn "prop_composeCons':"
  quickCheck (prop_compareCons' :: String -> String -> Bool)
  putStrLn "prop_composeConcat"
  quickCheck (prop_compareConcat :: [String] -> Bool)
  putStrLn "prop_take:"
  quickCheck (prop_take :: Int -> String -> Bool)
  putStrLn "prop_read:"
  quickCheck (prop_read :: Int -> Bool)
  putStrLn "idem1:"
  quickCheck idem1
  putStrLn "idem2:"
  quickCheck (idem2 :: String -> Bool)
