module Evaluate where

-- evaluates to 1
one = const 1 undefined

-- bottom
two = const undefined 1

-- evals to 1
three = flip const undefined 1

-- bottom
four = flip const 1 undefined

-- bottom
five = const undefined undefined

-- evals to 'a'
six = foldr const 'z' ['a'..'e']

-- evals to 'z'
seven = foldr (flip const) 'z' ['a'..'e']
