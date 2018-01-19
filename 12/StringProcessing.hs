--12/StringProcessing.hs
module StringProcessing where

notThe :: String -> Maybe String
notThe str = if str == "the" then Nothing else Just str

wurdz :: String -> [String]
wurdz str = (fst a):(if (snd a) == [] then [] else (wurdz $ tail $ snd a))
  where a = break ((==) ' ') str

wurdzMap ::  (String -> String) -> String -> String
wurdzMap f str = (f (fst a)) ++ (if (snd a) == [] 
                                 then "" 
                                 else " " ++ (wurdzMap f $ tail $ snd a))
  where a = break ((==) ' ') str

replaceThe :: String -> String
replaceThe str = init $ go (words str) where
  go [] = []
  go (x:xs) = (if notThe x == Nothing then "a" else u $ notThe x) 
                ++ " " ++ (go xs)
  u (Just a) = a

replaceThe' :: String -> String
replaceThe' str = init $ concatMap (g . f) $ wurdz str where
  g x = ((++) x " ")
  f x = (if notThe x == Nothing then "a" else u $ notThe x)
  u (Just a) = a

replaceThe2 :: String -> String
replaceThe2 str = wurdzMap (f .notThe) str where
  f (Nothing) = "a"
  f (Just a) = a

-- 2

isVowel :: Char -> Bool
isVowel c = (elem c "aeiouAEIOU")

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go (words str) 0 where
  go [] n = n
  go [x] n = n
  go (x:(c:cs):xss) n = if (x == "the" && (isVowel c)) 
                       then go xss (n + 1) 
                       else go xss n

countVowels :: String -> Integer
countVowels str = foldr f 0 str where 
  f x y = if isVowel x then y + 1 else y

countConsonants :: String -> Integer
countConsonants str = foldr f 0 str where 
  f x y = if not $ isVowel x then y + 1 else y

-- Validate the word

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = if countConsonants str >= countVowels str
             then (Just (Word' str)) else Nothing



