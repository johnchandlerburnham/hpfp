module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar key string = go key $ (map toLower . filter isAlpha) string
  where go _ "" = ""
        go n (c:cs) = chr ((ord c + n - ord 'a') `mod` 26 + ord 'a') : go n cs

unCaesar :: Int -> String -> String
unCaesar key string = caesar (negate key) string

test :: Int -> String -> Bool
test n s = (map toLower . filter isAlpha) s == (unCaesar n . caesar n) s

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
