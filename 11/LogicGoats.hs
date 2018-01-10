{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

--10/LogicGoats.hs
module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, str) = n > 42 || (length str) > 42

-- instance TooMany (Int, Int) where
-- tooMany (n, m) = n + m > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = tooMany (n + m)

newtype Goats = Goats Int deriving (Eq, Show, TooMany)



