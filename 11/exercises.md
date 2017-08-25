# Exercises for Chapter 11: Algebraic datatypes

# Exercises: Dog Types

1. type constructor
2. * -> *
3. *
4. Num a => Doggies a
5. Doggies Integer
6. Doggies String
7. data constructor (both?)
8. a -> DogueDeBordeaux a
9. DogueDeBordeaux String

# Exercises: Vehicles

price.hs
price2.hs

# Exercises: Cardinality

1. cardinality is 1
2. 3
3. 2^16 = 65536
4. 2^64
5. 8 bit number...

## Exercises: For Example

1. MakeExample's type is Example, Example does not have a type, it is a type
2. Example has data constructor MakeExample with an instance of typeclass Show
3. MakeExample is a function from Int to Example

## Exercises: Logic Goats

see toomany.hs

## Exercises: Pity the Bool

1. 4, Bool has cardinality 2 and there are 2 Bools in a sum type
2. 258, the type can either be BoolyBool True, BoolyBool False or a Numba. 
   you get a compiler waring -Woverflowedliterals

## Exercises: How Does Your Garden Grow?

1. 
```
data Garden = Gardenia String 
            | Daisy String 
            | Rose String
            | Lilac String
            deriving Show
```

## Exercises: Programmers

programmer.hs

## Exercises: The Quad

1. 8
2. 16
3. 4^4 = 256
4. 2*2*2 = 8
5. 2^2^2 = 16
6. (4^4)^2 = 65536

## BinaryTree
binarytree.hs

## Chapter Exercises

### Multiple Choice

1. a
2. c
3. b
4. c.

### Ciphers
vignere.hs

### As-patterns 
aspatterns.hs

### Language exercises
aspatterns.hs

### Phone exercise

phone.hs

### Hutton's Razor

hutton.hs
