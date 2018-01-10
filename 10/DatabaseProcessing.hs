module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1 
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map unpack . filter isADbDate where
  isADbDate (DbDate _) = True
  isADbDate _ = False
  unpack (DbDate utc) = utc

filterDbDate2 :: [DatabaseItem] -> [UTCTime]
filterDbDate2 = foldr unpackDate [] where
  unpackDate (DbDate utc) acc = utc : acc
  unpackDate _ acc = acc

-- 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr unpackNum [] where
  unpackNum (DbNumber num) acc = num : acc
  unpackNum _ acc = acc

-- 3 
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = foldr compareDate base db where
  compareDate (DbDate utc) acc = max utc acc  
  compareDate _ acc = acc
  base = (filterDbDate2 db) !! 0

-- 4 
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr addNum 0 where
  addNum (DbNumber int) acc = int + acc
  addNum _ acc = acc

-- 5
avgDb :: [DatabaseItem] -> Double 
avgDb db = (fromIntegral $ fst tup) / (fromIntegral $ snd tup) where
  tup = foldr addNum (0,0) db
  addNum (DbNumber int) (num,den) = (int + num, den + 1)
  addNum _ acc = acc

avgDb2 :: [DatabaseItem] -> Double
avgDb2 db = n / d where
  n = (fromIntegral $ sumDb db) 
  d = (fromIntegral $ length $ filterDbNumber db)


