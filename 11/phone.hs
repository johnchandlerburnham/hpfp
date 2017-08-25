module Phone where

import Data.List
import Data.Char

data Phone = Phone {buttons :: [Button]} deriving (Eq, Show)

data Mode = Shift | None deriving (Eq, Show)
data Button = Button {key :: Key, output :: String} deriving (Eq, Show)
type Key = Char 
type Press = Int

daPhone :: Phone
daPhone = Phone 
          [ Button '1' "1"    , Button '2' "ABC2", Button '3' "DEF3"
          , Button '4' "GHI4" , Button '5' "JKL5", Button '6' "MNO6"
          , Button '7' "PQRS7", Button '8' "TUV8", Button '9' "WXYZ9"
          , Button '*' "^*"   , Button '0' " 0"  , Button '#' ".,#"
          ]

convo = ["Wanna play 20 questions", 
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tated alcohol lol",
         "Lol ya",  
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty lol",
         "lol ya",
         "Haha thanks just makin sure rofl ur turn"]

keyMap :: Phone -> (Key, Press) -> Mode -> Char
keyMap phone (k, p) mode = 
  if mode == Shift then out else toLower out where
    out = outCycle !! ((p - 1) `mod` (length outCycle))
    outCycle = unpack $ lookup k $ zip (map key pb) (map output pb)
    pb = (buttons phone)
    unpack (Just a) = a

textOut :: Phone -> [(Key, Press)] -> String
textOut phone kps = go phone kps None where
  go phone [] _ = [] 
  go phone (('*', 1):kps) None = go phone kps Shift
  go phone (('*', 1):kps) Shift = go phone kps None
  go phone (kp:kps) m =  ((keyMap phone) (shift kp) m) : go phone kps None
  shift (k, p) = if k == '*' then (k, p - 1) else (k, p)

invButton :: Button -> [(Char, (Key, Press))]
invButton b = go ((key b), (output b)) 1 where
  go (k, []) _ = []
  go (k, (c:cs)) n = (c, (k, n)) : go (k, cs) (n + 1)

invKeyMap :: Phone -> Char -> Maybe (Key, Press)
invKeyMap phone c = lookup c $ concatMap invButton $ buttons daPhone

keyPressIn :: Phone -> String -> [Maybe (Key, Press)]
keyPressIn phone [] = [] 
keyPressIn phone (x:xs) = if isUpper x then up else lo where
  up = (Just ('*', 1)):(invKeyMap phone x):(keyPressIn phone xs)
  lo = (invKeyMap phone (toUpper x)):(keyPressIn phone xs)

fingerTaps :: [Maybe (Key, Press)] -> Press
fingerTaps a = go a 0 where
  go [] n = n
  go ((Nothing):xs) n = go xs n
  go ((Just (k, p)):xs) n = go xs (p + n)

mostPopularLetter :: String -> Char
mostPopularLetter str = head $ maximumBy cmp $ group str where
  cmp a b = compare (length a) (length b)

-- costOfMostPopularLetter :: Phone -> String -> Press
costOfMostPopularLetter phone str = fingerTaps $ keyPressIn phone [a] where
  a = mostPopularLetter str

coolestLtr :: [String] -> Char
coolestLtr msgs = mostPopularLetter $ map mostPopularLetter msgs

coolestWord :: [String] -> String
coolestWord msgs = head $ maximumBy cmp $ group $ concatMap words msgs where 
  cmp a b = compare (length a) (length b)




