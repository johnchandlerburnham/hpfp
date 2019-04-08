# 4 Basic Datatypes

## 4.2 What are types?

**Types**: Haskell has expressions. Type the number `1` into the REPL That's an
expression. Type `addOne = (+) 1`. The function `addOne` is also an expression.
Try to imagine all the possible expressions we could type into GHCi. This is
hard to do because the number of possible expressions is infinite.  But if we
try to imagine lots of different expressions, we should start to notice
patterns. `1` is an expression, so is `2`, so is `3`, and so on. All positive
integers are expressions. `-1` is an expression, so `-2` and `-3`. Negative
integers are expressions. `0` is an expression, therefore all integers are
expressions.  The pair `(1,1)` is an expression, so is `(1,2)`, so is
`(23,58982)`. All pairs of integers are expressions. We can keep going like this
forever, finding new patterns of ways to group expressions together. Every time
we find a new expression-pattern, if we can precisely describe the structure of
that pattern, we have a type.

When we played with the `String` type in the preceding chapter, we were, in
effect, saying "Let's for the moment think about only those expressions that
have the `String` pattern, which looks like this:

```haskell
data String = [Char]
data [] a = [] | a : [a]
```

Haskell mandates that expressions have types, and the compiler will not let us
run code where the types do not match up.

Try running:

```haskell
Prelude> not "foo"

<interactive>:4:5: error:
    • Couldn't match expected type ‘Bool’ with actual type ‘[Char]’
    • In the first argument of ‘not’, namely ‘"foo"’
      In the expression: not "foo"
      In an equation for ‘it’: it = not "foo"
Prelude> :type not
not :: Bool -> Bool
```

`not` is a function which takes a `Bool` and returns a `Bool`. If we
try to call `not` with a `String` we get a type error. Our expression
was not "well-typed."

We can also define new patterns, like

```haskell
data Grocery = Milk | Eggs | Flour
```

So the type system is a tool for defining new patterns in the space
of possible expressions, and then checking that in the code we want to
run, all the types fit together perfectly.

If you have ever played with Legos, you already have an intuition for
how this ought to work.

<p align="center">
<img
src="https://upload.wikimedia.org/wikipedia/commons/3/32/Lego_Color_Bricks.jpg"
width = 400px
alt="Legos">
</p>



There are a lot of different ways to fit Lego' together. Two standard two by
four Lego bricks of the same color can be combined 24 ways (ignoring
symmetries). But there are also a lot of ways that you can't fit pieces
together. You can't, for example, place a brick on top of two adjacent bricks
at different heights. No amount of force will get the pieces to bend (Lego's are
very tough) that way. You can't "coerce" Lego's into doing whatever you want.
The shapes are what they are, and it's up to you the builder to figure out
some interesting way to fit them together.

Haskell expressions are like Lego pieces. And types are like their shapes.
But unlike with Lego's, you get to design entirely new pieces, as well
as put them together.

## 4.3 Anatomy of a data declaration

In the data declaration:

```haskell
data Bool = False | True
```

It's important to keep in mind that everything to left of the `=` are
types, and everything to the right are expressions.

```haskell
Prelude> data Bool = False | True deriving Show
Prelude> False
False
Prelude> Bool

<interactive>:6:1: error: Data constructor not in scope: Bool
Prelude> :type False
False :: Bool
Prelude> :info Bool
data Bool = False | True  -- Defined at <interactive>:4:1
instance [safe] Show Bool -- Defined at <interactive>:4:35
Prelude>
```

`Bool` and `False` live in two different spaces. `Bool` lives in type-space
and `False` lives in data-space. This is a really important distinction!
Type-space disappears after code gets compiled, so you can't interact with
them in running code (or "runtime").

**compile-time**: When code gets compiled. Types are used in compile-time,
but not in runtime. Compiler errors happen at compile-time.

**run-time**: When code gets run. Haskell types vanish at run-time. A run-time
error might be an exception like:

```haskell
Prelude> head []
*** Exception: Prelude.head: empty list
```

Another thing to remember is that since type-space and data-space are distinct,
the same name can live in both spaces:

```haskell
Prelude> data Thing a = Thing a deriving Show
Prelude> Thing 1
Thing 1
Prelude> Thing 1 :: Thing Integer
Thing 1
```

`Thing` the data constructor lives in data-space:

```haskell
Prelude> :t Thing
Thing :: a -> Thing a
```

And `Thing` the type constructor lives in type-space:

```haskell
Prelude> :i Thing
data Thing a = Thing a  -- Defined at <interactive>:24:1
instance [safe] Show a => Show (Thing a)
  -- Defined at <interactive>:24:33
```

But this is just two names that happen to be the same. We could equivalently
say:

```haskell
Prelude> data Thing a = MakeThing a deriving Show
```
and everything behaves the same:

```haskell
Prelude> data Thing a = MakeThing a deriving Show
Prelude> Thing 1

<interactive>:5:1: error:
    Data constructor not in scope: Thing :: Integer -> t
Prelude> MakeThing 1
MakeThing 1
Prelude> :t Thing

<interactive>:1:1: error: Data constructor not in scope: Thing
Prelude> :t MakeThing
MakeThing :: a -> Thing a
Prelude> :i Thing
data Thing a = MakeThing a  -- Defined at <interactive>:4:1
instance [safe] Show a => Show (Thing a)
  -- Defined at <interactive>:4:37
Prelude>
```

### Exercises: Mood Swing
1. `Mood`
2. `Blah` or `Woot`
3. Woot is a value whose type is Mood, should be `changeMood :: Mood -> Mood`
4. [See `Mood.hs`](/04/Mood.hs)
5. Ibid.

## 4.4 Numeric types

Numeric types will not completely make sense without typeclasses.

**Typeclass**: A collection of types that share common properties.
For example, the typeclass `Show` is defined as

```haskell
Prelude> :i Show
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
  -- Defined in ‘GHC.Show’
```

Which is for our purposes equivalent to:

```haskell
class Show a where
  show :: a -> String
```

So any type that is an instance of `Show` has a function called `show`
that lets you turn a value of that type into a `String`.

Let's try it:

```haskell
Prelude> data Something = Something
Prelude> instance Show Something where show Something = "Something"
Prelude> show Something
"Something"
```

Of course, this is tedious, so Haskell gives us a `deriving` mechanism
that does effectively this:

```haskell
Prelude> data Something = Something deriving Show
Prelude> show Something
"Something"
```

The reason this is relevant is that `Num`, `Fractional` and `Integral` are
all typeclasses, not types:

```haskell
Prelude> :i Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
    -- Defined in ‘GHC.Num’

Prelude> :i Fractional
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  {-# MINIMAL fromRational, (recip | (/)) #-}
    -- Defined in ‘GHC.Real’
Prelude> :i Integral
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  {-# MINIMAL quotRem, toInteger #-}
    -- Defined in ‘GHC.Real’
```

But this is getting pretty deep into the "typeclass zoo." Better leave this
for chapters 5 and 6.

## 4.5 Comparing Values

Let's think about what a comparison is. Things are different from one another,
sometimes in a lot of different ways. An apple can be red, crisp and sweet
while an orange can be orange, fleshy and tart. It's tough to compare two things
when they differ in a lot of different ways, hence the expression "you can't
compare apples and oranges."

But actually, you can compare apples and oranges, and as long as you restrict
the comparison to a single dimension of difference, it's pretty easy. A
particular apple a particular orange both have size, so you can say one is
bigger than the other. And that's a comparison! Color, taste, texture, ripeness,
country of origin, there are loads of dimensions in which a comparison could
make sense.

Let's be even more constrained for a moment and think about what equality is.
What does it mean for something to be the same as something else? Again, it's
easier to think about this if we only consider one dimension of difference at a
time. Our apple and orange both have weight, and those weights can be the same,
or different.

Let's model this by making a type `Fruit` which can be either an `Apple` or and
`Orange`, each of which contains an `Int` that represents their weight:

```haskell
> data Fruit = Apple Int | Orange Int
```

Lets make a particular apple that weighs 4 units, and an orange that weighs 5
units"

```
> apple  = Apple 4
> orange = Orange 5
```

In general, the question of what it means for two things to be equal is a really
subtle and interesting one. Here, our common sense and knowledge of arithmetic
says `5` is bigger than `4`, so the `orange` is not the same weight but is
actually bigger than the `apple`.

Let's ask GHCi if the `apple` is equal to the `orange`:

```haskell
Prelude> apple == orange

<interactive>:11:1: error:
    • No instance for (Eq Fruit) arising from a use of ‘==’
    • In the expression: apple == orange
      In an equation for ‘it’: it = apple == orange
```

This is saying, in so many words: "Is `apple` equal to `orange`? That depends on
what the meaning of the word ~~is~~ equals is." Equals is defined through the
`Eq` typeclass:

```haskell
Prelude> :i Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

For any type `a`, an equals is a function that takes two `a`s and a returns a
`Bool`. If they're equal, then `(==)` returns `True`, otherwise `False` (`(/=)`
is the negation "not equals").

Let's see what happens if we tell GHCi to derive an `Eq` instance for `Fruit`,
basically to do a default thing that makes sense:

```haskell
> data Fruit = Apple Int | Orange Int deriving Eq
> apple = Apple 4
> orange = Orange 5
> apple == orange
False
```

Okay, that seems to be what we expected, but let's see what happens if we make
both fruits the same size:

```haskell
> apple = Apple 4
> orange = Orange 4
> apple == orange
False
```

When we told GHCi to just figure out an equality function, it didn't have any
way of knowing that the `Int` inside the `Apple` and `Orange` data constructors
was the only thing we cared about, so it derived an `(==)` function that also
takes the data constructors themselves into account.

```haskell
> apple = Apple 4
> orange = Orange 4
> apple == apple
True
```

If we only want `(==)` to consider the weight `Int` we have to write our own
`Eq` instance:

```haskell
module Fruit where

data Fruit = Apple Int | Orange Int

instance Eq Fruit where
  (==) a b = (weight a) == (weight b)

weight :: Fruit -> Int
weight (Apple a) = a
weight (Orange a) = a
```

If we load this into GHCi, this should now do what we expect:

```
> (Apple 4) == (Orange 4)
True
```

By the way, we could have gotten around having to write the cumbersome `weight`
function if we had used Haskell's record syntax in our `Fruit` data constructors:

```haskell
data Fruit = Apple { weight :: Int } | Orange { weight :: Int }
```

This automatically generates the weight functions we want.

The `Ord` typeclass is very much like `Eq`, but is focused on "bigness" rather
than equality:

```haskell
> :i Ord
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
```

It's called `Ord` because it's short for "Orderable", as in "can be put in
order." I like expanding typeclass names by adding and "-able" to the end of
them. `Eq` is the class of "Equable" types, `Show` is the class of "Showable"
types, etc. etc.

## 4.6 Go on and Bool me

Okay, one very very pattern in Haskell is the overlapping language used for
logical disjunction (OR) and addition, and logical conjunction (AND) and
multiplication. A type (like `Bool`) that can be either one thing (like `True`)
or another (like `False`) is called a "sum" type. A type (like a tuple `(a, b)`
that has to have one thing and another is called a "product" type. This language
comes from a branch of math called category theory, but is actually much less
scary than it seems at first. The basic idea is that when you try to count (or
enumerate) all the possible values that can be in a type, an OR (in Haskell a
`|`) in your constructor acts like adding number of possibilities on both sides
of the disjunction, whereas an AND acts like multiplying the possibilities on
both sides of the conjunction. Hence, "sum" for adding, and "product" for
multiplying.

Haskell doesn't have a special syntax for product types, it just puts the two
parts of the next to each other separated by a space, so `a b` is the product
type of `a` and `b`, while `a | b` is the sum type of `a` and `b`.

This concept will be explained more in later chapters.

### Exercises: Find the Mistakes

1. `not True && True`
2. `not (x == 6) where x = 5`
3. `(1 * 2) > 5`
4. `["Merry"] > ["Happy"]`
5. `["1, 2, 3"] ++ "look at me!"`


## 4.9 Chapter Exercises

1. `length :: [a] -> Int`

2.  a. 5
    b. 3
    c. 2
    d. 5

3. `Int` is not a `Fractional`
4. Use infix ```div` `` instead
5. `Bool`, returns `True`
6. `Bool`, returns `False`
7.  a. Works, `False`
    b. Error, no instance `(Num Char)`
    c. Works, returns `8`
    d. Works returns `False`
    e. No instance `(Num Bool)`

8. [See `Exercises.hs`.](/04/Exercises.hs)
9. Ibid.
10. Ibid.

### Correcting syntax

[See `CorrectingSyntax.hs`.](/04/CorrectingSyntax.hs)

### Match the function names to their types

1.  c.
2.  b.
3.  a.
4.  d.
