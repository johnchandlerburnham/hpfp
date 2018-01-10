--10/WarmUp.hs
module WarmUp where

-- 1
stops = "pbtdkg"
vowels = "aeiou"

-- 1a
stopVowelStop = [(a, b, c) | a <- stops, b <- vowels, c <- stops]

-- 1b
pVowelStop = [('p', b, c) | b <- vowels, c <- stops]

-- 1c
nouns = ["cat", "dog", "ball", "box"]
verbs = ["throws", "catches", "jumps", "fetches"]

nounVerbNoun =   [(a, b, c) | a <- nouns, b <- verbs, c <- nouns]

-- 2
seekritFunc x = div (sum (map length (words x))) (length (words x))

-- function is average word length
avgWordLength :: String -> Int
avgWordLength x = div totalWordLengths numberOfWords 
  where
    wordList = words x
    numberOfWords = length wordList 
    wordLengths = map length wordList
    totalWordLengths = sum wordLengths

-- 3
preciseAvgWordLength :: String -> Double
preciseAvgWordLength x = totalWordLength / numberOfWords
  where 
    totalWordLength = fromIntegral $ sum $ map length $ words x
    numberOfWords   = fromIntegral $ length $ words x
