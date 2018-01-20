--22/WarmingUp.hs
module WarmingUp where

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  c <- cap
  r <- rev
  return (c, r)

tupledBind :: [Char] -> ([Char], [Char])
tupledBind str = cap <$> rev >>= (,) $ str

tupledBind' :: [Char] -> ([Char], [Char])
tupledBind' = rev >>= (\x -> cap >>= \y -> return (x, y))

{- 
instance Monad (-> r) where
  return = const
  (>>=) x f = \r -> f (x r) r

    (rev >>= (\x -> cap >>= \y -> return (x, y))) str
 -> (\x -> cap >>= \y -> return (x, y)) (rev str) str
 -> (cap >>= \y -> return (rev str, y)) str
 -> (\y -> return (rev str, y)) (cap str) str
 -> (return (rev str, cap str)) str
 -> (rev str, cap str)
-}
