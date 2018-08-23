module FoldBool where

foldBool = map (\x -> (Data.Bool.bool x (-x) (x == 3)))
