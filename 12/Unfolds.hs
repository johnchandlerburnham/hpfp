--12/Unfolds.hs
module Unfolds where

import Data.List

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f s = go f s (f s) where 
  go f s Nothing = []
  go f s (Just (a, b)) = a : go f b (f b)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f s = myUnfoldr (g f) s where
  g f s = Just (s, (f s))
  


