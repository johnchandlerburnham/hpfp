-- 08/DividedBy.hs
module DividedBy where

unsafeDividedBy :: Integral a => a -> a -> (a, a)
unsafeDividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

-- div throws an exception on zero
partialDividedBy :: Integral a => a -> a -> (a, a)
partialDividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | d < 0 = go n (negate d) count
      | d == 0 = error $ "can't div by zero"
      | otherwise = go (n - d) d (count + 1)


data DividedResult = Result Integer | DividedByZero deriving (Eq, Show)
-- equivalent to Maybe Integer

dividedBy :: Integral a => a -> a-> DividedResult
dividedBy num denom = go num denom 0
  where
    resNeg (Result x) = Result (negate x)
    go n d count
      | d == 0 = DividedByZero
      | d < 0 = resNeg $ go n (negate d) count 
      | n < 0 = resNeg $ go (negate n) d count 
      | n < d = Result count
      | otherwise = go (n - d) d (count + 1)

