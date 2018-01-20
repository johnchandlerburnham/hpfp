--20/ChapterExercises.hs
module ChapterExercises where

import Data.Foldable
import Data.Monoid

-- 1
data Constant a b = Constant b deriving Show

instance Foldable (Constant a) where
  foldMap _ _ = mempty

-- 2
data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

-- 3
data Three a b c = Three a b c 

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

-- 4  
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = (f b) <> (f c) 

-- 5
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = (f b) <> (f c) <> (f d)


--- filterF
filterF :: (Applicative f, Foldable t, Monoid (f a)) 
           => (a -> Bool) -> t a -> f a
filterF f xs = foldMap (g f) xs where
  g f a = if f a then pure a else mempty 
