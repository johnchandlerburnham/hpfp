module TypeKwonDo where

-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

-- 2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n a = (+ fromIntegral n) (f a)
