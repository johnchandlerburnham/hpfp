module Cipher where

import Data.Char
import System.Exit (exitSuccess)

caesar :: Int -> String -> String
caesar key string = go key $ (map toLower . filter isAlpha) string
  where go _ "" = ""
        go n (c:cs) = chr ((ord c + n - ord 'a') `mod` 26 + ord 'a') : go n cs

unCaesar :: Int -> String -> String
unCaesar key string = caesar (negate key) string 

test :: Int -> String -> Bool
test n s = (map toLower . filter isAlpha) s == (unCaesar n . caesar n) s 

encrypt :: IO ()
encrypt = do
  putStrLn "Cleartext:"
  clearText <- getLine
  putStrLn "Key: " 
  key <- getLine
  putStrLn "Ciphertext is: "
  print (caesar (read key) clearText)

