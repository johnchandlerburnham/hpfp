--13/CipherIO.hs
module CipherIO where

import Data.Char
import System.Exit (exitSuccess)

caesar :: Int -> String -> String
caesar key string = go key $ (map toLower . filter isAlpha) string
  where go _ "" = ""
        go n (c:cs) = chr ((ord c + n - ord 'a') `mod` 26 + ord 'a') : go n cs

unCaesar :: Int -> String -> String
unCaesar key string = caesar (negate key) string 

vignere :: String -> String -> String
vignere key cleartext = map caeserHelper $ zip key' clr' where
    pre = (map toLower . filter isAlpha)
    clr' = (pre cleartext)
    key' = take (length clr') $ cycle (pre key)
    caeserHelper (a, b) = chr ((ord a + ord b - 2*ord 'a') `mod` 26 + ord 'a')

unVignere :: String -> String -> String
unVignere key ciphertext = map caeserHelper $ zip key' ciphertext where
    pre = (map toLower . filter isAlpha)
    key' = take (length ciphertext) $ cycle (pre key)
    caeserHelper (a, b) = chr ((ord b - ord a) `mod` 26 + ord 'a')

testCaesar :: Int -> String -> Bool
testCaesar n s = (map toLower . filter isAlpha) s == (unCaesar n . caesar n) s 

testVignere :: String -> String -> Bool
testVignere key s = 
  (map toLower . filter isAlpha) s == (unVignere key . vignere key) s 

encrypt :: IO ()
encrypt = do
  putStrLn "Cleartext:"
  clearText <- getLine
  putStrLn "Caeser Key: " 
  cKey <- getLine
  putStrLn "Vignere Key: " 
  vKey <- getLine
  putStrLn "Caesar Ciphertext is: "
  print (caesar (read cKey) clearText)
  putStrLn "Vignere Ciphertext is: "
  print (vignere vKey clearText)

