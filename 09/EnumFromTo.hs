--09/EnumFromTo.hs

module EnumFromTo where

-- the exercises for specific types

eftBool:: Bool -> Bool -> [Bool]
eftBool x y = go x y []
  where 
    go a b c
      | a > b = c
      | a == b = reverse (a : c)
      | otherwise = go (succ a) b (a : c)

eftInt:: Int -> Int -> [Int]
eftInt x y = go x y []
  where 
    go a b c
      | a > b = c
      | a == b = reverse (a : c)
      | otherwise = go (succ a) b (a : c)

eftOrd:: Ordering -> Ordering -> [Ordering]
eftOrd x y = go x y []
  where 
    go a b c
      | a > b = c
      | a == b = reverse (a : c)
      | otherwise = go (succ a) b (a : c)

eftChar:: Char -> Char -> [Char]
eftChar x y = go x y []
  where 
    go a b c
      | a > b = c
      | a == b = reverse (a : c)
      | otherwise = go (succ a) b (a : c)

-- these all look basically the same, so let's generalize:

eft :: (Ord a, Enum a) => a -> a -> [a]
eft x y = go x y []
  where 
    go a b c
      | a > b = c
      | a == b = reverse (a : c)
      | otherwise = go (succ a) b (a : c)

-- that reverse is clunky though, let's see if we can get rid of it

eft2 :: (Ord a, Enum a) => a -> a -> [a]
eft2 x y = go x y []
  where 
    go a b c
      | a > b = c
      | a == b = c ++ a:[]
      | otherwise = go (succ a) b (c ++ a:[])

-- using string concatenation is just as slow though, let's use recursion

eft3 :: (Ord a, Enum a) => a -> a -> [a]
eft3 x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : eft3 (succ x) y

-- We can get rid of the Ord constraint by leveraging the mapping between
-- an Enum and Int

eft4 :: Enum a => a -> a -> [a]
eft4 x y
  | fromEnum x > fromEnum y = []
  | fromEnum x == fromEnum y = [x]
  | otherwise = x : eft4 (succ x) y



