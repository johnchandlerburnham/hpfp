module RecursiveMult where

rmult :: (Integral a) => a -> a -> a
rmult a b = go a b 0
  where 
    go a b c
      | b == 0 = c
      | otherwise = go a (b - 1) (c + a)
