module ListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f (Cons a as) = Cons (f a) (fmap f as)
  fmap _ Nil = Nil

instance Applicative List where
  pure x = Cons x Nil
  (<*>) _  Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) fs as = flatMap (flip fmap as) fs
--  (<*>) (Cons f fs) as = (fmap f as) `append` ((<*>) fs as)
--  (<*>) fs as = flatMap (\f -> fmap f as) fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    as <- arbitrary
    return (mkList as)

instance Eq a => EqProp (List a) where
  (=-=) = eq

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat' a
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (zipWith' fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    as <- arbitrary
    return (ZipList' as)

zipWith' :: List (a -> b) -> List a -> List b
zipWith' Nil _ = Nil
zipWith' _ Nil = Nil
zipWith' (Cons f fs) (Cons x xs) = Cons (f x) $ zipWith' fs xs

take' :: Int -> List a -> List a
take' n xs = go n Nil xs where
  go 0 ys _ = ys
  go n ys Nil = ys
  go n ys (Cons x xs) = go (n - 1) (Cons x ys) xs

repeat' :: a -> List a
repeat' a = Cons a $ (repeat' a)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

mkList :: [a] -> List a
mkList xs = foldr Cons Nil xs

main :: IO ()
main = do
  let test = [(1, 2, 3), (4, 5, 6)] :: [(Int, Int, Int)]
  quickBatch $ applicative (mkList test)
  quickBatch $ applicative (ZipList' $ mkList test)
