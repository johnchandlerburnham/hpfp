module Phone where

import Data.List
import Data.Char

-- 1
data Phone = Phone { buttons :: [Button] } deriving (Eq, Show)
data Mode = Uppercase | Lowercase deriving (Eq, Show)
data Button = ModeButton | KeyButton { key :: Key, output :: String } 
              deriving (Eq, Show)
type Key = Char 
type Presses = Int

-- 2
phone' = Phone buttons'
buttons' = [ Button '1' ""    , Button '2' "ABC", Button '3' "DEF" 
           , Button '4' "GHI" , Button '5' "JKL", Button '6' "MNO" 
           , Button '7' "PQRS", Button '8' "TUV", Button '9' "WXYZ" 
           , Button '*' "^"   , Button '0' " " , Button '#' ".," 
           ]

convo :: [String]
convo = ["Wanna play 20 questions", 
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tated alcohol lol",
         "Lol ya",  
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty lol",
         "lol ya",
         "Haha thanks just makin sure rofl ur turn"]

keyMap :: Phone -> [(Key, String)]
keyMap ph = map g $ zip (map key $ buttons ph) (map output $ buttons ph)
  where g (a, b) = (a, b ++ [a])

tap :: Phone -> Mode -> (Key, Presses) -> Char
tap ph m (k, pr) = if m == Lowercase then toLower out else out where
        out = outCycle !! ((pr - 1) `mod` (length outCycle))
        outCycle = unpack $ lookup k (keyMap ph)
        unpack (Just a) = a

tapsOut :: Phone -> [(Key, Presses)] -> String
tapsOut ph kps = go ph kps Lowercase where
  go ph [] _ = [] 
  go ph (('*', 1):kps) Lowercase = go ph kps Uppercase
  go ph (('*', 1):kps) Uppercase = go ph kps Lowercase
  go ph (kp:kps) m = taps ph m (handleStar kp) : go ph kps m
  handleStar (k, p) = if k == '*' then (k, p - 1) else (k, p)

-- reverseTaps :: Phone -> Char -> [(Key, Presses)]
-- reverseTaps = 

-- findButton :: Phone -> Char -> Digit
-- findButton p c = zip (map digit pButtons) (map output pButtons) where
-- pButtons = buttons p




