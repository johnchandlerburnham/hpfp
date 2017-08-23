# Exercises for Chapter 4: Basic datatypes


## Exercises: Mood Swing
1. `Mood`
2. `Blah` or `Woot`
3. Woot is a value whose type is Mood, should be `changeMood :: Mood -> Mood`
4. see `mood.hs`
5. see `mood.hs`

## Exercises: Find the Mistakes

1. `not True && True`
2. `not (x == 6) where x = 5`
3. `(1 * 2) > 5`
4. `["Merry"] > ["Happy"]`
5. `["1, 2, 3"] ++ "look at me!"`

## Chapter Exercises

1. `length :: [a] -> Int`

2.
- a. 5
- b. 3
- c. 2
- d. 5

3. `Int` is not a `Fractional`
4. Use infix ```div` `` instead
5. `Bool`, returns `True`
6. `Bool`, returns `False`
7. 
- a. Works, `False`
- b. Error, no instance `(Num Char)`
- c. Works, returns `8`
- d. Works returns `False`
- e. No instance `(Num Bool)`

8. 
   ```
   isPalindrome :: (Eq a) => [a] -> Bool
   isPalindrome x = reverse x == x
   ```

9.
   ``` 
   myAbs :: Integer -> Integer
   myAbs = if x < 0 then (-x) else x
   ```

10. see `ex10.hs`

### Correcting syntax

1. See `correctingsyntax1.hs`
2. See `correctingsyntax2.hs`
3. See `correctingsyntax3.hs`

### Match the function names to their types

1. c.
2. b.
3. a
4. d

