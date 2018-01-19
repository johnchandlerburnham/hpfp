--17/Constant.hs

module Constant where

newtype Constant a b = 
  Constant { getConstant :: a } deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap f (Constant a) = (Constant a)

instance Monoid a => Applicative (Constant a) where
  pure f = (Constant mempty)
  (<*>) (Constant a) (Constant b) = Constant (a `mappend` b)
