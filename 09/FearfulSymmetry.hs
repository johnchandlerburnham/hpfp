--09/FearfulSymmetry.hs
module FearfulSymmetry where

split :: String -> [String]
split [] = []
split  x = word : split rest
  where 
    word = takeWhile (/= ' ') x
    rest = (drop 1) $ dropWhile (/= ' ') x

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn  c str = part : (splitOn c rest)
  where 
    part = takeWhile (/= c) str
    rest = (drop 1) $ dropWhile (/= c) str

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines x = splitOn '\n' x

shouldEqual = 
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
