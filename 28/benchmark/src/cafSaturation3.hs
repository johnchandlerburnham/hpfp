module Main where

incdInts :: [Integer] -> [Integer]
incdInts = map (+1)

main :: IO ()
main = do
  print (incdInts [1..] !! 1000)
