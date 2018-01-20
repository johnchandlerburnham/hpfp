--22/ReadingComprehension.hs
{-# LANGUAGE InstanceSigs #-}
module ReadingComprehension where

import Control.Monad
import Control.Applicative

newtype Reader r a = Reader { runReader :: r -> a }

-- 1
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

-- 2
asks :: (r -> a) -> Reader r a
asks f = Reader f

--3
instance Functor (Reader r) where
  fmap f (Reader r) = Reader $ f . r

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r (ra r)

-- Reader Monad
-- 1
instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) aRb = Reader $ \r -> runReader (aRb (ra r)) r


