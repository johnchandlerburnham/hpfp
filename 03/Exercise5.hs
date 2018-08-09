module Reverse where

rvrs :: String -> String
rvrs x = third ++ second ++ first where
         third = drop 9 x
         second = take 4 $ drop 5 x
         first = take 5 x

main :: IO ()
main = print $ rvrs "Curry is awesome"
