module Fruit where

data Fruit = Apple Int | Orange Int

instance Eq Fruit where
  (==) a b = (weight a) == (weight b)

weight :: Fruit -> Int
weight (Apple a) = a
weight (Orange a) = a

