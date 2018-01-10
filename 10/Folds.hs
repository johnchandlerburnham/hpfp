module Folds where

-- 1: 
one = (==) (foldr (*) 1 [1..5]) (foldl (*) 1 [1..5])

-- 2
two = scanl (flip (*)) 1 [1..3]

--3: c

--4: reduce structure

--5
fiveA = foldr (++) "" ["woot", "WOOT", "woot"]
fiveB = foldr max 'a' "fear is the little death"
fiveC = foldr (&&) True [False, True]
fiveD = foldr (||) True [False, True]
fiveE = foldr ((++) . show) "" [1..5]
fiveF = foldl const 'a'  [1..5]
fiveG = foldl const 0 "tacos"
fiveH = foldr (flip const) 0 "burritos"
fiveI = foldr (flip const) 'z' [1..5]


