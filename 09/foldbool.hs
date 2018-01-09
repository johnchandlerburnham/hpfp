--09/foldbool.hs
foldBool = map (\x -> (Data.Bool.bool x (-x) (x == 3)))
