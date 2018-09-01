{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

{-
-- this instance is bad, makes functor identity fail

instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
 (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

-- this works 
instance (Applicative n, EqProp (n a), EqProp a) => EqProp (S n a) where
 (S x y) =-= (S p q) = ((=-=) x p) .&. (y =-= q)
-}

-- since S already has an Eq instance, this seems not unreasonable
instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S x y) = S (f <$> x) (f y)

instance Foldable n => Foldable (S n) where
  foldMap f (S x y) = (foldMap f x) `mappend` (f y)
 
instance Traversable n => Traversable (S n) where
  traverse f (S x y) = S <$> (traverse f x) <*> (f y)

type IIL = (Int, Int, [Int])

main = do
  -- sample (arbitrary :: Gen (S [] Int))
  -- verboseBatch (functor (undefined :: S [] IIL))
  quickBatch (traversable (undefined :: S [] IIL))
