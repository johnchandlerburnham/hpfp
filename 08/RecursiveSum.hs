module RecursiveSum where

rsum :: (Eq a, Num a) => a -> a
rsum n = go n 0
  where
    go n c
      | n == 0 = c
      | otherwise = go (n - 1) (c + n)
