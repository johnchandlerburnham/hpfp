--15/Mem.hs
module Mem where 

import Data.Monoid
import Test.QuickCheck

newtype Mem s a = Mem { runMem :: s -> (a , s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\n -> (mempty, n))
  mappend (Mem f) (Mem g) = Mem h where
    h = (\n -> ((fst $ f n) <> (fst $ g n), snd $ f $ snd $ g n))

f' = Mem $ \s -> ("hi", s + 1)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
