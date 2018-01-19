--16/FunctorInstances.hs
module FunctorInstances where

import Test.QuickCheck
import Test.QuickCheck.Function

-- 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do 
    a' <- arbitrary 
    return (Identity a')

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- 2 
data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do 
    a <- arbitrary 
    a' <- arbitrary 
    return (Pair a a')

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a' <- arbitrary  
    b' <- arbitrary
    return (Two a' b')

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a' <- arbitrary  
    b' <- arbitrary
    c' <- arbitrary
    return (Three a' b' c')

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- 5
data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary  
    b <- arbitrary
    b' <- arbitrary
    return (Three' a b b')

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

-- 6
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

-- 7
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

-- Testing
type IntToInt = Fun Int Int

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

main :: IO ()
main = do
  quickCheck (functorIdentity :: (Identity Int) -> Bool)
  quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Identity Int) -> Bool)
  quickCheck (functorIdentity :: (Pair Int) -> Bool)
  quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Pair Int) -> Bool)
  quickCheck (functorIdentity :: (Two Int Int) -> Bool)
  quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Two Int Int) -> Bool)
  quickCheck (functorIdentity :: (Three Int Int Int) -> Bool)
  quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Three Int Int Int) -> Bool)
  quickCheck (functorIdentity :: (Three' Int Int) -> Bool)
  quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Three' Int Int) -> Bool)
  quickCheck (functorIdentity :: (Four Int Int Int Int) -> Bool)
  quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Four Int Int Int Int) -> Bool)
  quickCheck (functorIdentity :: (Four' Int Int) -> Bool)
  quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Four' Int Int) -> Bool)


