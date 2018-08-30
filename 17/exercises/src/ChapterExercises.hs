module ChapterExercises where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Specialize
-- 1
listPure :: a -> [a]
listPure = pure

listApply :: [(a -> b)] -> [a] -> [b]
listApply = (<*>)

-- 2
ioPure :: a -> IO a
ioPure = pure

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply = (<*>)

-- 3
tuplePure  :: (Monoid a, Monoid c) => a -> (c, a)
tuplePure = pure

tupleApply :: (Monoid a, Monoid c) => (c, (a -> b)) -> (c, a) -> (c, b)
tupleApply = (<*>)

-- 4
funcPure :: a -> (e -> a)
funcPure = pure

funcApply :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
funcApply = (<*>)

-- Instances
-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Pair a a')

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a' <- arbitrary
    b' <- arbitrary
    return (Two a' b')

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two a b) (Two c d) = Two (a <> c) (b d)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a' <- arbitrary
    b' <- arbitrary
    c' <- arbitrary
    return (Three a' b' c')

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a b c) (Three e f g) = Three (a <> e) (b <> f) (c g)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a b c) (Three' e f g) = Three' (a <> e) (b f) (c g)

instance (Eq a, Eq b) => EqProp (Three' a b ) where
  (=-=) = eq

-- 5

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d) where
    arbitrary = do
      a' <- arbitrary
      b' <- arbitrary
      c' <- arbitrary
      d' <- arbitrary
      return (Four a' b' c' d')

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four a b c d) (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d h)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
      a <- arbitrary
      a' <- arbitrary
      a'' <- arbitrary
      b <- arbitrary
      return (Four' a a' a'' b)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' a b c d) (Four' e f g h) = Four' (a <> e) (b <> f) (c <> g) (d h)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
-- combos a b c = liftA3 (\a b c -> (a, b, c)) a b c
combos a b c = liftA3 (,,) a b c

-- Testing

test = (1, 2, 3)

main :: IO ()
main = do
  let test = ("a", "b", "c")
  quickBatch $ applicative (Pair test test)
  quickBatch $ applicative (Two test test)
  quickBatch $ applicative (Three test test test)
  quickBatch $ applicative (Three' test test test)
  quickBatch $ applicative (Four test test test test)
  quickBatch $ applicative (Four' test test test test)
