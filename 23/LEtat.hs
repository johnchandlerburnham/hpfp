--23/LEtat.hs
{-# LANGUAGE InstanceSigs #-}
module LEtat where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  -- fmap f (Moi g) =  Moi $ (\(x, y) -> (f x, y)) . g
  fmap f (Moi g) =  Moi $ \s0 -> let (a, s1) = (g s0) in (f a, s1)

instance Applicative (Moi s) where
  pure :: a -> Moi s a 
  pure a = Moi (\s0 -> (a, s0))
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi g) = 
    Moi $ \s0 -> let (a, s1) = (g s0); (f', s2) = (f s1) in (f' a, s2)

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (>>=) (Moi f) g = 
    -- Moi $ \s0 -> let (a, s1) = (f s0); (Moi gg) = (g a) in (gg s1)
    Moi $ \s0 -> let (a, s1) = (f s0) in (runMoi (g a)) s1
  
