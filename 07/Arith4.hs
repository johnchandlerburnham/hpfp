module Arith4 where

-- 4
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = (read . show)

-- 6
roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 a = read (show a)

main :: IO ()
main = do
  print (roundTrip (4 :: Int))
  print (roundTrip 4 :: Int)
  print (roundTripPF (4 :: Int))
  print (roundTripPF 4 :: Int)
  print (roundTrip2 4 :: Int)
  print (id 4)



