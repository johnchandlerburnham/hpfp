module EqInstances where

-- 1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = (==) x y

-- 2
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  Two x y == Two p q = (x, y) == (p, q)

-- 3
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  TisAnInt x   == TisAnInt y   = x == y
  TisAString x == TisAString y = x == y
  _ == _ = False

-- 4
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  Pair a b == Pair x y = (a, b) == (x, y)

-- 5
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple x y = (a, b) == (x, y)

-- 6
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  ThisOne x == ThisOne y = x == y
  ThatOne x == ThatOne y = x == y
  _ == _ = False

-- 7
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello x == Hello y = x == y
  Goodbye x == Goodbye y = x == y
  _ == _ = False
