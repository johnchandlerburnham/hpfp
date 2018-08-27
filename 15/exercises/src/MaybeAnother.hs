module MaybeAnother where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend Nada (Only x) = (Only x)
  mappend (Only x) Nada = (Only x)
  mappend (Only x) (Only y) = (Only (mappend x y))

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    oneof [return (First' (Only a)), return (First' Nada)]

instance Monoid (First' a ) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = (First' Nada)
  mappend (First' Nada) (First' (Only x)) = (First' (Only x))
  mappend (First' (Only x)) _ = (First' (Only x))

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

