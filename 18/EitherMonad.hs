module EitherMonad where

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

instance Applicative (Sum a) where
  pure b = Second b
  (<*>) _           (First e) = First e
  (<*>) (First e) _           = First e
  (<*>) (Second f) (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  (>>=) (Second b) f = (f b)


