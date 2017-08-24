module FoldBool where

import Data.Bool

foldBool = map (\x -> (bool x (-x) (x == 3)))
