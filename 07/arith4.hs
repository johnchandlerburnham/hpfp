module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = (read . show)

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 a = read (show a)

main = do 
  print (roundTrip2 (4 :: Int))
  print (id 4)



