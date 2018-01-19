--12/Natural.hs
module Natural where

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger nat = go nat 0 
  where
    go Zero n = n
    go (Succ a) n = go a (n + 1)
  

integerToNat :: Integer -> Maybe Nat
integerToNat n 
  | n < 0 = Nothing
  | otherwise = (Just $ go n Zero) 
  where
    go 0 nat = nat
    go n nat = go (n - 1) (Succ nat)
  
