module Scans where

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

--1
fibsToN n = take n $ fibs

--2
fibsLessThan n = takeWhile (< n) fibs

--3
factorials = 1 : scanl ratio 1 factorials
  where ratio m n = (m `div` n + 1) * m

factorials2 = scanl (*) 1 [1..]
lazyCaterers = scanl (+) 1 [1..]


