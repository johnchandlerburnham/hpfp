--18/monadexercises/src/ChapterExercises.hs
module ChapterExercises where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where 
  (=-=) = eq

-- 2 
data PhEither b a = Right' b | Left' a deriving (Eq, Show)

instance Functor (PhEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap f (Right' b) = Right' b

instance Applicative (PhEither b) where
  pure a = Left' a
  (<*>) (Right' b) _ = Right' b
  (<*>) _ (Right' b) = Right' b
  (<*>) (Left' f) (Left' a) = Left' (f a)

instance Monad (PhEither b) where
  return = pure
  (>>=) (Left' a) f  = (f a)
  (>>=) (Right' b) f  = Right' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhEither b a)  where
  arbitrary = do
    b' <- arbitrary
    a' <- arbitrary  
    oneof [return (Left' b'), return (Right' a')]

instance (Eq b, Eq a) => EqProp (PhEither b a) where 
  (=-=) = eq

-- 3
data Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f  = (f a)

instance Arbitrary a => Arbitrary (Identity a)  where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Eq a => EqProp (Identity a) where 
  (=-=) = eq

-- 4
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil 
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  -- (<*>) fs as = toList $ [f a | f <- (fromList fs), a <- (fromList as)]
  (<*>) fs as = fold append Nil $ fmap (\f -> fmap f as) fs 

instance Monad List where
  return = pure
  -- (<*>) fs as = fold append Nil $ fmap (\f -> fmap f as) fs 
  --(>>=) as f = toList $ [b | a <- (fromList as), b <- (fromList $ f a)]
  (>>=) as f = fold append Nil $ fmap f as 

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

fromList :: List a -> [a]
fromList Nil = [] 
fromList (Cons a as) = a:(fromList as)

toList :: [a] -> List a
toList [] = Nil 
toList (x:xs) = Cons x (toList xs)


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    as <- arbitrary 
    return (toList as)

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- Write functions

-- 1
j :: Monad m => m (m a) -> m a
j x = x >>= id

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

-- 4
a :: Monad m => m a -> m (a -> b) -> m b 
a = flip (<*>) 

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return [] 
meh (a:as) f = liftM2 (:) (f a) (meh as f)

-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id


-- Testing

-- type SSS = (String, String, String)
type III = (Int, Int, Int)

main = do
  quickBatch $ functor (NopeDotJpg :: Nope III)
  quickBatch $ applicative (NopeDotJpg :: Nope III)
  quickBatch $ monad (NopeDotJpg :: Nope III)
  quickBatch $ functor (undefined :: PhEither III III)
  quickBatch $ applicative (undefined :: PhEither III III)
  quickBatch $ monad (undefined :: PhEither III III)
  quickBatch $ functor (undefined :: Identity III)
  quickBatch $ applicative (undefined :: Identity III)
  quickBatch $ monad (undefined :: Identity III)
  quickBatch $ functor (undefined :: List III)
  quickBatch $ applicative (undefined :: List III)
  quickBatch $ monad (undefined :: List III)
