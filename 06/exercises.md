# Exercises for Chapter 6: Typeclasses

## Exercises: Eq Instances

1. `instance Eq TisAnInteger where (==) TisAn x TisAn y = (==) x y`
2. `instance Eq TwoIntegers where Two x y == Two p q = (x, y) == (p, q)`
3. String or Int
   ```
   instance Eq StringOrInt where 
   TisAnInt x == TisAnInt y = x == y
   TisAString x == TisAString y = x == y
   _ == _ = False
   ```
4. `instance Eq Pair where Pair a b == Pair x y = (a, b) == (x, y)`
5. `instance Eq Tuple where Tuple a b == Tuple x y = (a, b) == (x, y)`
6. Which
   ``` 
   instance Eq a => Eq (Which a) where 
   ThisOne x == ThisOne y = x == y
   ThatOne x == ThatOne y = x == y
   _ == _ = False
   ```
7. EitherOr
   ```
   instance (Eq a, Eq b) => Eq (EitherOr a b) where 
   Hello x == Hello y = x == y
   Goodbye x == Goodbye y = x == y 
   _ == _ = False
   ```
 
## Exercises: Tuple Experiment

`quotRem` and `divMod` return a tuple with the values from `quot` and `rem` or 
`div` and `mod` respectively.

## Exercises: Will They Work?

1. `5`
2. `LT`
3. error, a string and a bool are not comparable
4. `False`

## Chapter Exercises

### Multiple choice

1. c
2. b
3. a
4. c
5. a

### Does it typecheck?:

1. ch6/ex1.hs 
2. ch6/ex2.hs
3. 
  a. any `Mood`, i.e. `Blah` or `Woot`
  b. type error, `9` is not a `Mood`
  c. `Mood` does not derive `Ord`

4.ch6/ex4.hs

### Given a datatype declaration, what can we do?
exdatatype.hs

1. `"chases"` and `True` are a `String` and a `Bool`, not a `Rocks` and a `Yeah`
2. works
3. works
4. `Papu` isn't an instance of `Ord`

### Match the types:

exmatch1.hs and exmatch2.hs

1. Since `i = 1`, `i` has to be a `Num`, it can't be a type that `1` isn't,
like e.g.  a `String`. We can't cast `i` upwards.
2. `1.0` is not just any instance of `Num`, the syntax implies `Fractional`.
3. works
4. works
5. works, we can always cast downwards
6. works
7. Doesn't work, `sigmund` returns `myX` which is an `Int`
8. Doesn't work, `sigmund'` returns an `Int` not any instance of `Num`
9. Works, restricts `jung` to a list of `Ints` rather than any list
10. Works, restricts input to `String`
11. Doesn't work, `mySort` only sorts `Strings`, not any instance of `Ord`

### Type-Kwon-Do: Electric Typealoo

1. `chk f a b = f a == b`
2. `arith f n a = (+ fromIntegral n) (f a)`


