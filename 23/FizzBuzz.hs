--23/FizzBuzz.hs
module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod`  5 == 0 = "Buzz"
           | n `mod`  3 == 0 = "Fizz"
           | otherwise       = show n

main0 :: IO ()
main0 = mapM_ (putStrLn . fizzBuzz) [1..100]

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  put ((fizzBuzz n) : xs)

main1 :: IO ()
main1 = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]

fizzbuzzList' :: [Integer] -> DL.DList String
fizzbuzzList' list = execState (mapM_ addResult' list) DL.empty

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  put (DL.snoc xs (fizzBuzz n)) 

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzList' [1..100]

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo a b = fizzbuzzList [b,(b - 1)..a]

main' :: IO ()
main' = mapM_ putStrLn $ fizzbuzzFromTo 1 100



