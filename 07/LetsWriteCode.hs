module LetsWriteCode where

-- 1 a.  `tensDigit x = (flip mod) 10 $ fst $ divMod x 10`
tensDigit :: Integral a => a -> a
tensDigit x = d
  where
   xLast = x `div` 10
   d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = let (a, b) = divMod x 10 in a `mod` 10

-- Seems silly to use `divMod`, when this works perfectly well.

tensDigit'' :: Integral a => a -> a
tensDigit'' x = x `div` 10 `mod` 10

-- 1 b.  Yes

-- 1 c.  Well let's do this properly with a function thats general for any base
baseDigit :: Integral a => a -> a -> a -> Maybe a
baseDigit base digit x
  | base == 0 = Nothing
  | digit < 0 = Nothing
  | otherwise = Just $ x `div` (base ^ digit) `mod` base

{- So that
*Main> baseDigit 10 2 1234
Just 2
*Main> baseDigit 10 2 123456
Just 4
*Main> baseDigit 10 2 9876543210
Just 2

In other words, the fundamental theorem of arithmetic has any number N equal
the polynomial:

N = a_0 * b^0 + a_1 * b^1 ...

The base digit function gives the a_k, or a_digit coefficient for a given number
and base.
-}

-- 2
foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
   True -> x
   False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
 | b == True = x
 | b == False = y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, b) = (f a, b)

