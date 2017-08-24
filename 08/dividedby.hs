module DividedBy where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)


dividedBy2 :: Integral a => a -> a -> (a, a)
dividedBy2 num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | d < 0 = go n (negate d) count
          | d == 0 = error $ "can't div by zero"
          | otherwise = go (n - d) d (count + 1)

data DividedResult = Result Integer | DividedByZero deriving (Eq, Show)

dividedBy3 :: Integral a => a -> a-> DividedResult
dividedBy3 num denom = go num denom 0
  where go n d count
          | d == 0 = DividedByZero
          | d < 0 = resNeg $ go n (negate d) count 
          | n < 0 = resNeg $ go (negate n) d count 
          | n < d = Result count
          | otherwise = go (n - d) d (count + 1)
            where resNeg (Result x) = Result (negate x)

