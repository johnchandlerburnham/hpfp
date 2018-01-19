--12/EitherLib.hs
module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' es = foldr f [] es where
  f (Left x) acc = x:acc
  f _ acc = acc

rights' :: [Either a b] -> [b]
rights' es = foldr f [] es where
  f (Right x) acc = x:acc
  f _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c 
either' f g (Left a) = f a
either' f g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (const Nothing) (Just . f) e
