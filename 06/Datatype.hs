module Datatype where

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- 1
-- `"chases"` and `True` are a `String` and a `Bool`, not a `Rocks` and a `Yeah`
-- so we have to wrap them up in their data constructors
phew = Papu (Rocks "chases") (Yeah True)


-- 2
-- works
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3
-- works
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4
-- `Papu` isn't already instance of `Ord`, so we have to define one:
instance Ord Papu where
  compare (Papu (Rocks s) (Yeah b)) (Papu (Rocks s') (Yeah b')) =
    case (compare b b') of
      EQ -> compare s s'
      LT -> LT
      GT -> GT

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
