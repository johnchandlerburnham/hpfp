module Identity where

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure f = (Identity f)
  (<*>) (Identity f) (Identity a) = Identity (f a)

