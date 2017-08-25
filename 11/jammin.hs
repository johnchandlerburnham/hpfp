--1
module Jammin where
import Data.List

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Show, Ord)

--2 
data JamJars = Jam { fruit :: Fruit, number :: Int } deriving (Eq, Show)

--4
instance Ord JamJars where
  compare (Jam _ a) (Jam _ b) = compare a b

--3 cardinality is 2^64 * 4

--5 
row1 = Jam Peach 1
row2 = Jam Apple 2
row3 = Jam Blackberry 3
row4 = Jam Plum 4
row5 = Jam Peach 5
row6 = Jam Plum 6
allJam = [row1, row2, row3, row4, row5, row6]

-- 6
numberOfJams = sum $ map number allJam

-- 7
mostRow = maximum allJam

-- 8

-- 9

compareKind (Jam k _) (Jam k' _) = compare k k'

jamKinds = groupBy eqKind $ sortBy compareKind allJam where
  eqKind a b = compareKind a b == EQ



