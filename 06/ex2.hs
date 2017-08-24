module Ex2 where

data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x
