module Tree where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Data.Monoid

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left a right) = Node (f <$> left) (f a) (f <$> right)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = (f a)
  foldMap f (Node left a right) = (foldMap f left) <> (f a) <> (foldMap f right)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> (f a)
  traverse f (Node left a right) = 
    Node <$> (traverse f left) <*> (f a) <*> (traverse f right)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [ return Empty
                     , Leaf <$> arbitrary
                     , Node <$> arbitrary <*> arbitrary <*> arbitrary
                     ]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

-- Testing
type IIL = (Int, Int, [Int])

main = do 
  quickBatch (traversable (undefined :: Tree IIL))
