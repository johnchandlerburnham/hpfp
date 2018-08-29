--17/applicativeexercises/src/VariationEither.hs
module VariationEither where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation' e a = Failure' e | Success' a deriving (Eq, Show)

instance Functor (Validation' e) where
  fmap f (Success' a) = Success' (f a)
  fmap f (Failure' e) = Failure' e

instance Monoid e => Applicative (Validation' e) where
  pure a = Success' a
  (<*>) (Failure' e) (Failure' r) = Failure' (e `mappend` r)
  (<*>) _           (Failure' e) = Failure' e
  (<*>) (Failure' e) _           = Failure' e
  (<*>) (Success' a) (Success' b) = Success' (a b)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation' e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    oneof [return (Failure' e), return (Success' a)]

instance (Eq e, Eq a) => EqProp (Validation' e a) where
  (=-=) = eq

main :: IO ()
main = do
  let test :: Validation' (String, String, String) (String, String, String)
      test = Success' ("a", "b", "c")
  quickBatch $ applicative test 


