module Exercises where

-- 1
data Person = Person Bool
instance Show Person where show (Person x) = show x

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2
data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

-- 3
-- a. any `Mood`, i.e. `Blah` or `Woot`
-- b. type error, `9` is not a `Mood`
-- c. `Mood` does not derive `Ord`

-- 4
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool" "spit"
s2 = Sentence "Julie" "loves" "dogs"
