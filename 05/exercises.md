# Exercises for Chapter 5: Types

## Exercises: Type Matching

1 & 2:
- a. not :: Bool -> Bool
- b. length :: [a] -> Int
- c. concat :: [[a]] -> [a]
- d. head :: [a] -> a
- e. (<)  :: Ord a => a -> a -> Bool


## Exercises: Type Arguments 

1. a 
2. d
3. d
4. c
5. a
6. e
7. e
8. e
9. c

## Exercises: Parametricity

1. This is impossible because id has to work for a type that only one member.
   If a type only has one member, then the only thing a function with 
   signature a -> a can do if passed a value of that type is return the same
   value (or bottom, which is in every type) without breaking the type
   signature.
2. `f x y = x` or `f x y = y`
3. `a -> b -> b` is the same as `a -> (b -> b)` and the only thing with type
`(b -> b)` is the id function. So this function is a kind of constant
function that takes two arguments and returns the second, as opposed to 
`const :: a -> b -> a` which takes two arguments and returns the first.
One implementation would be `const id`, but I am unsure whether `flip const`
counts as a separate implementation.

## Exercises: Apply Yourself

1. `[Char] -> [Char]`
2. `Fractional a => a -> a`
3. `Int -> [Char]`
4. `Int -> Bool`
5. `Char -> Bool`

## Chapter Exercises

### Multiple Choice
1. c
2. a
3. b
4. c

### Determine the type:

1. 
- a. `Num a => a`
- b. `Num a => (a, [Char])`
- c. `(Integer, [Char])`
- d. `Bool`
- e. `Int`
- f. `Bool`

2. `Num a => a`
3. `Num a => a -> a`
4. `Fractional a => a`
5. `[Char]`

### Does it compile?:

1. `bignum $ 10` doesn't make sense `5^10` is a number not a function
2. This should work.
3. c and d need a function.
4. c not in scope.

### Type variable or specific type constructor?

1. 
- 0: constrained polymorphic type var
- 1: fully polymorphic type var
- 2: concrete
- 3: concrete

2. 
- 0: fully polymorphic
- 1: concrete
- 2: concrete

3. 
- 0: fully polymorphic
- 1: constrained polymorphic
- 2: concrete

4. 
- 0: fully polymorphic 
- 1: fully polymorphic
- 2: concrete

### Write a type signature:

1. `[a] -> a`
2. `(Ord a, Ord b) => a -> b -> Bool`
3. `(a, b) -> b`

### Given a type, write the function:

1. `i = id`
2. `c x y = x`
3. `yes`
4. `c' x y = y`
5. `r = tail`
6. `co x y z = x $ y z`
7. `a x y = fst(y, x y)`
8. `a' x y = x y`

### Fix it

1. see fixit1.hs
2. see fixit2.hs
3. see arith3broken.hs

### Type-Kwon-Do

1. `h x = g $ f x`
2. `e x = w $ q x`
3. `xform (x, y) = (xz x, yz y) `
4. `munge f g x = fst $ g $ f x`

