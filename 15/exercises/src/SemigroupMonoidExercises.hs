module SemigroupMonoidExercises where

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe
import Data.Semigroup
import qualified Data.Monoid as M

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial 

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>) 

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do 
    a' <- arbitrary 
    return (Identity a')

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a1) <> (Identity a2) = (Identity (a1 <> a2))

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity a) (Identity b) = Identity ((M.<>) a b)

-- instance Semigroup (Identity a) where
--  (Identity a) <> _ = (Identity a)

type IdentAssoc = Identity String -> Identity String -> Identity String -> Bool 

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a' <- arbitrary  
    b' <- arbitrary
    return (Two a' b')

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a1 b1) (Two a2 b2) = Two ((M.<>) a1 a2) ((M.<>) b1 b2)

type TwoAssoc = 
  (Two String String) -> (Two String String) -> (Two String String) -> Bool 

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a' <- arbitrary  
    b' <- arbitrary
    c' <- arbitrary
    return (Three a' b' c')

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

type ThreeAssoc = (Three String String String) -> 
                  (Three String String String) -> 
                  (Three String String String) -> 
                  Bool 

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => 
  Arbitrary (Four a b c d) where
    arbitrary = do
      a' <- arbitrary  
      b' <- arbitrary
      c' <- arbitrary
      d' <- arbitrary
      return (Four a' b' c' d')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => 
  Semigroup (Four a b c d) where
    (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = 
      Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

type FourAssoc = (Four String String String String) -> 
                 (Four String String String String) -> 
                 (Four String String String String) -> 
                 Bool 

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary :: Gen Bool
    return (BoolConj a)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = (BoolConj True)
  _ <> _ = (BoolConj False)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj True) (BoolConj True) = (BoolConj True)
  mappend _ _ = (BoolConj False)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary :: Gen Bool
    return (BoolDisj a)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = (BoolDisj False)
  _ <> _ = (BoolDisj True)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj False) (BoolDisj False) = (BoolDisj False)
  mappend _ _ = (BoolDisj True)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8 
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a' <- arbitrary  
    b' <- arbitrary
    oneof [return (Fst a'), return (Snd b')]

instance Semigroup (Or a b) where
  (Snd b) <> _       = (Snd b)
  _       <> (Snd b) = (Snd b)
  _       <> (Fst a) = (Fst a)

type OrAssoc = 
  (Or String String) -> (Or String String) -> (Or String String) -> Bool 

-- 9 
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Show a, Show b) => Show (Combine a b) where 
  show _ = "Combine"

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\n -> (f n) <> (g n))

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\n -> mempty) 
  mappend (Combine f) (Combine g) = Combine (\n -> (M.<>) (f n) (g n))

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = fmap Combine $ promote (\n -> coarbitrary n arbitrary)

combineAssoc :: (Combine String String) -> 
                (Combine String String) -> 
                (Combine String String) -> 
                String -> 
                Bool 
combineAssoc a b c s = 
  (unCombine (a <> (b <> c)) s) == (unCombine ((a <> b) <> c) s)

combineLeftIdentity :: (Combine String String) -> String -> Bool
combineLeftIdentity a s = ((unCombine (mempty M.<> a)) s) == ((unCombine a) s)

combineRightIdentity :: (Combine String String) -> String -> Bool
combineRightIdentity a s = ((unCombine (a M.<> mempty)) s) == ((unCombine a) s)


-- 10 
newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where 
  show _ = "Comp"

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp id
  mappend (Comp f) (Comp g) = Comp (f . g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = fmap Comp $ promote (\n -> coarbitrary n arbitrary)

compAssoc :: (Comp String) -> (Comp String) -> (Comp String) -> String -> Bool 
compAssoc a b c s = (unComp (a <> (b <> c)) s) == (unComp ((a <> b) <> c) s)

compLeftIdentity :: (Comp String) -> String -> Bool
compLeftIdentity a s = ((unComp (mempty M.<> a)) s) == ((unComp a) s)

compRightIdentity :: (Comp String) -> String -> Bool
compRightIdentity a s = ((unComp (a M.<> mempty)) s) == ((unComp a) s)

-- 11 
data Validation a b = Fail a | Pass b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where 
  (Fail a) <> (Fail b) = (Fail (a <> b))
  _        <> (Fail a) = (Fail a)
  (Fail a) <> _        = (Fail a)
  _        <> (Pass b) = (Pass b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary  
    b <- arbitrary
    oneof [return (Fail a), return (Pass b)]

type ValidationAssoc = (Validation String String) -> 
                       (Validation String String) -> 
                       (Validation String String) -> 
                       Bool 

-- 12 
newtype AccRight a b = AccRight (Validation a b) deriving (Eq, Show)

instance (Semigroup b) => Semigroup (AccRight a b) where
  (AccRight (Pass a)) <> (AccRight (Pass b)) = AccRight (Pass (a <> b))
  (AccRight (Fail a)) <> _                   = AccRight (Fail a)
  _                   <> (AccRight (Fail b)) = AccRight (Fail b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccRight a b) where
  arbitrary = do
    a <- arbitrary  
    b <- arbitrary
    oneof [return (AccRight (Fail a)), return (AccRight (Pass b))]

type AccRightAssoc = (AccRight String String) -> 
                     (AccRight String String) -> 
                     (AccRight String String) -> 
                     Bool 

-- 13
newtype AccBoth a b = AccBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccBoth a b) where
  (AccBoth (Pass a)) <> (AccBoth (Pass b)) = AccBoth (Pass (a <> b))
  (AccBoth (Fail a)) <> (AccBoth (Fail b)) = AccBoth (Fail (a <> b))
  _                  <> (AccBoth (Fail b)) = AccBoth (Fail b)
  (AccBoth (Fail a)) <> _                  = AccBoth (Fail a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccBoth a b) where
  arbitrary = do
    a <- arbitrary  
    b <- arbitrary
    oneof [return (AccBoth (Fail a)), return (AccBoth (Pass b))]

type AccBothAssoc = (AccBoth String String) -> 
                    (AccBoth String String) -> 
                    (AccBoth String String) -> 
                    Bool 
-- Testing
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = ((M.<>) mempty a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = ((M.<>) a mempty) == a

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck combineAssoc
  quickCheck combineLeftIdentity
  quickCheck combineRightIdentity
  quickCheck compAssoc
  quickCheck compLeftIdentity
  quickCheck compRightIdentity
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccRightAssoc)
  quickCheck (semigroupAssoc :: AccBothAssoc)


