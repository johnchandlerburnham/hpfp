--11/Vignere.hs
module Vignere where

import Data.Char

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

