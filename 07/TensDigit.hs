-- 07/TensDigit.hs
module TensDigit where

tensDigit x = (flip mod) 10 $ fst $ divMod x 10
