--12/MaybeLib.hs
module MaybeLib where

-- 1
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just a) = True

isNothing :: Maybe a -> Bool 
isNothing = not . isJust 

-- 2 
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f (Just a) = (f a) 
mayybee b f Nothing = b

-- 3
fromMaybe :: a -> Maybe a -> a 
fromMaybe a m = mayybee a id m

-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = (Just x)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a] 

-- 5
catMaybes :: [Maybe a] -> [a]
catMaybes [] = [] 
catMaybes ((Just a):xs) = a : (catMaybes xs)
catMaybes (Nothing:xs) = catMaybes xs 

-- 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe ms = go ms [] where
  go [] acc = Just acc
  go ((Just x):xs) acc = go xs (x:acc)
  go (Nothing:xs) _ = Nothing



