--20/LibraryFunctions.hs
module LibraryFunctions where

import Prelude hiding 
  (sum, product, elem, minimum, maximum, null, length)
import Data.Foldable hiding 
  (sum, product, elem, minimum, maximum, null, length, toList, fold)
import Data.Monoid 

-- 1
sum' :: (Foldable t, Num a) => t a -> a
sum' xs = foldr (+) 0 xs

sum :: (Foldable t, Num a) => t a -> a
sum xs = getSum $ foldMap Sum xs 

-- 2
product' :: (Foldable t, Num a) => t a -> a
product' xs = foldr (*) 1 xs 

product2 :: (Foldable t, Num a) => t a -> a
product2 xs = getSum $ foldMap Sum xs 

-- 3
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x xs = foldr ((||) . (== x)) False xs

elem2 :: (Foldable t, Eq a) => a -> t a -> Bool
elem2 x xs = getAny $ foldMap (Any . (== x)) xs

-- 4
newtype Least a = Least { getLeast :: Maybe a } deriving (Eq, Ord, Show)

instance Ord a => Monoid (Least a) where
  mempty = Least Nothing
  mappend (Least Nothing) a = a
  mappend a (Least Nothing) = a
  mappend (Least (Just a)) (Least (Just b)) = Least (Just (min a b))
   

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs = getLeast $ foldMap (Least . Just) xs
 
-- 5 
newtype Most a = Most { getMost :: Maybe a } deriving (Eq, Ord, Show)

instance Ord a => Monoid (Most a) where
  mempty = Most Nothing
  mappend (Most Nothing) a = a
  mappend a (Most Nothing) = a
  mappend (Most (Just a)) (Most (Just b)) = Most (Just (max a b))
   
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs = getMost $ foldMap (Most . Just) xs

-- 6
newtype Null a = Null {getNull :: Bool} deriving (Eq, Show)

instance Monoid (Null a) where
  mempty = Null True
  mappend (Null True) (Null True) = Null True
  mappend _ _ = Null False

null :: (Foldable t) => t a -> Bool
null xs = getNull $ foldMap (Null . (const False)) xs

-- 7
newtype Long a = Long {getLong :: Int} deriving (Eq, Show)

instance Monoid (Long a) where
  mempty = Long 0
  mappend (Long a) (Long b) = Long (a + b)

length :: (Foldable t) => t a -> Int
length xs = getLong $ foldMap (Long . (const 1)) xs

-- 8 
toList :: (Foldable t) => t a -> [a]
toList xs = foldMap (:[]) xs

-- 9
fold :: (Foldable t, Monoid m) => t m -> m
fold xs = foldMap id xs

-- 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f xs = foldr ((<>) . f mempty xs  
