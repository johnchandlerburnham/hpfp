--17/Lookups.hs
module Lookups where

import Data.List (elemIndex)

-- 1
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2 
y :: Maybe Integer
y = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

tupled2 :: Maybe (Integer, Integer)
tupled2 = (pure (,)) <*> y <*> z

-- that's cool, the first tupled builds the function ((,) y) inside the maybe
-- the second tupled lifts (,) inside the maybe then applies both args to it

-- 3
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = (pure max') <*> x3 <*> y3

-- 4
xs = [1, 2, 3]
ys = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = (pure sum) <*> ((,) <$> x4 <*> y4)

summed2 :: Maybe Integer
summed2 = sum <$> ((,) <$> x4 <*> y4)
