module Ex4 where

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool" "spit"
s2 = Sentence "Julie" "loves" "dogs"
