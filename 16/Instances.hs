--16/Instances.hs
{-# LANGUAGE FlexibleInstances #-}
module Instances where

-- 1
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ (Desk a) = (Desk a)
  fmap _ Finance = Finance

-- 2
data K a b = K a 

instance Functor (K a) where
  fmap _ (K a) = (K a)

-- 3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show) 

newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a)) 

-- 4
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5 
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa ) = LiftItOut (fmap f fa)

-- 6 
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa gb) = DaWrappa (fmap f fa) (fmap f gb)

-- 7

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 8
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap f (Cons a l) = Cons (f a) (fmap f l)
  fmap _ Nil = Nil

-- 10

data GLord a = NoG | OneG a | MoreG (GLord a) (GLord a) (GLord a)

instance Functor GLord where
  fmap f (MoreG g g' g'') = MoreG (fmap f g) (fmap f g') (fmap f g'')
  fmap f (OneG a) = OneG (f a)
  fmap _ NoG = NoG

-- 11
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap f (Read g) = Read (f . g)
  fmap f (Print s a) = Print s (f a)
  fmap _ Halt = Halt

