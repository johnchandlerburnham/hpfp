--21/traversable/src/TraversableInstances.hs
module TraversableInstances where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Data.Monoid

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
 fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = (f a)

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)  

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _  = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

-- Maybe

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap f Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap f Nada = mempty
  foldMap f (Yep a) = (f a)

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = fmap Yep (f a)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [Yep <$> arbitrary, return Nada]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show, Ord)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
   x <- arbitrary
   xs <- arbitrary
   oneof [return Nil, return (Cons x xs)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

data Three a b c = Three a b c deriving (Eq, Show, Ord)

instance Functor (Three a b) where
 fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = (f c)

instance Traversable (Three a b) where
  traverse f (Three a b c) = fmap (Three a b) (f c)  

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Pair

data Pair a b = Pair a b deriving (Eq, Show, Ord)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = (f b)

instance Traversable (Pair a) where
  traverse f (Pair a b) = fmap (Pair a) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- Big
data Big a b = Big a b b deriving (Eq, Show, Ord)

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldMap f (Big a b c) = (f b) <> (f c)

instance Traversable (Big a) where
  traverse f (Big a b c) = (Big a) <$> (f b) <*> (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show, Ord)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldMap f (Bigger a b c d) = (f b) <> (f c) <> (f d)

instance Traversable (Bigger a) where
  traverse f (Bigger a b c d) = (Bigger a) <$> (f b) <*> (f c) <*> (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- Testing
type IIL = (Int, Int, [Int])

main = do 
  quickBatch (traversable (undefined :: Identity IIL))
  quickBatch (traversable (undefined :: Constant IIL IIL))
  quickBatch (traversable (undefined :: Optional IIL))
  quickBatch (traversable (undefined :: List IIL))
  quickBatch (traversable (undefined :: Three IIL IIL IIL))
  quickBatch (traversable (undefined :: Pair IIL IIL))
  quickBatch (traversable (undefined :: Big IIL IIL))
  quickBatch (traversable (undefined :: Bigger IIL IIL))




