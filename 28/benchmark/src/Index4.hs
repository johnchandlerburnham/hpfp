module Index4 where

import Criterion.Main

myList :: [Int]
myList = [1..9999]

myList2 :: [Int]
myList2 = (undefined : [2..9999])

myList3 :: [Int]
myList3 = (undefined : undefined)

myList4 :: [Int]
myList4 = undefined

main :: IO ()
main = defaultMain
  [ bench "index list 9999 whnf" $ whnf (map (+1)) myList
  , bench "index list 9999 nf" $ nf (map (+1)) myList
  , bench "index list 9999" $ nf (map (+1)) myList2
  , bench "index list 9999" $ nf (map (+1)) myList3
  , bench "index list 9999" $ nf (map (+1)) myList4
  ]
