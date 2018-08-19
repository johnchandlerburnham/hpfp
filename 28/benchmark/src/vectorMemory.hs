module Main where

import Control.Monad.Primitive
import Control.Monad.ST
import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- Vector

vec :: V.Vector Int
vec = V.replicate 9998 0

-- Unboxed
uvec :: UV.Vector Int
uvec = UV.replicate 9998 0

main :: IO ()
main = defaultMain
  [ bench "slice vector" $ whnf (V.head . V.slice 1000 5000) vec
  , bench "slice unboxed vector" $ whnf (UV.head . UV.slice 1000 5000) uvec
  ]
