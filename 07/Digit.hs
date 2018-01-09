-- 07/Digit.hs

baseDigit :: Integral a => a-> a -> a -> Maybe a
baseDigit base digit x
  | base == 0 = Nothing
  | digit < 0 = Nothing
  | otherwise = Just $ (flip mod) base $ div x (base ^ digit)

