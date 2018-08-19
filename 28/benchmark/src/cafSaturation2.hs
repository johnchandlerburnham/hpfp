module Main where

incdInts :: [Integer] -> [Integer]
incdInts x = map (+1) x

main :: IO ()
main = do
  print (incdInts [1..] !! 1000)
