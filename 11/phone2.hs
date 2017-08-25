module Phone where

import Data.List
import Data.Char

-- 1
data Phone = Phone {keyMap :: KeyMap}
type KeyMap = (Key, Press) -> Mode -> Char
type Key = Char 
type Press = Int
data Mode = Uppercase | Lowercase deriving (Eq, Show)

buttons :: [(Char, String)]
buttons = [ ('1', ""    ), ('2', "ABC"), ('3', "DEF" )  
          , ('4', "GHI" ), ('5', "JKL"), ('6', "MNO" ) 
          , ('7', "PQRS"), ('8', "TUV"), ('9', "WXYZ") 
          , ('*', "^"   ), ('0', " " ), ('#', ".,"  ) 
          ]
          
buttons' :: [(Char, String)]
buttons' = map (\(x,y) -> (x, y ++ [x])) buttons

-- invButtons :: [(Char, (Char, String))]
invButtons = 

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

oldKeyMap :: KeyMap
oldKeyMap (key, press) mode = 
  if mode == Lowercase then toLower out else out where
    out = outCycle !! ((press - 1) `mod` (length outCycle))
    outCycle = unpack $ lookup key buttons
    unpack (Just a) = a

invKeyMap :: Char -> Maybe (Key, Press)
invKeyMap c = f (fmap fst x, (>>=) x (elemIndex c . snd)) where
  f (Just a, Just b) = Just (a, b) 
  f _ = Nothing
  x = find ((elem c) . snd ) buttons' where

textOutput :: Phone -> [(Key, Press)] -> String
textOutput phone kps = go phone kps Lowercase where
  go phone [] _ = [] 
  go phone (('*', 1):kps) Lowercase = go phone kps Uppercase
  go phone (('*', 1):kps) Uppercase = go phone kps Lowercase
  go phone (kp:kps) m =  ((keyMap phone) (shift kp) m) : go phone kps Lowercase
  shift (k, p) = if k == '*' then (k, p - 1) else (k, p)

{-
keypressInput :: Phone -> String -> [(Key, Press)]
keypressInput _ [] = []
keyPressInput phone (x:xs) = (f x) : go phone xs
-}
    
