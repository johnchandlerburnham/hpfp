--16/PossiblyEither.hs
module PossiblyEither where

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where 
  fmap f (Yeppers a) = Yeppers (f a)

data Sum a b = First a | Second b deriving (Eq, Show)


instance Functor (Sum a) where 
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a
