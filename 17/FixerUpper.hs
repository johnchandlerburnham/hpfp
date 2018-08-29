module FixerUpper where

-- 1
one = const <$> Just "Hello" <*> (pure "World")

-- 2
two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
