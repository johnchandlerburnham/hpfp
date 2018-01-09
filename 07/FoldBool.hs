-- 07/FoldBool.hs

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
    True -> x
    False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b 
  | b == True = x
  | b == False = y


