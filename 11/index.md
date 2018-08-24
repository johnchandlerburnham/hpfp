---
title: "Notes (HPFP 11/31): Algebraic Datatypes"
---

# 11 Algebraic Datatypes

## 11.3 Data and Type Constructors

Just like values are kind-of like zero-order functions because they take no
arguments, zero-order type and data constructors are like type values, or data
values. We can also say "constants" instead of values.

## 11.4 Type constructors and Kinds

Now that we know about type constants, or zero-order type constructors, we can
look at first and second order type constructors.

Remember that for functions the order of an expression is how many layers of
function the expression has. In the notes for Chapter 7, I wrote:

| Order | Description           | Example                      |
|-------|-----------------------|------------------------------|
| 0     | value, or literal     | "foobar", `3`                |
| 1     | function              | `((+) 3 4)`                  |
| 2     | function on functions | `map (+1) [1..10]`           |
| 3     | f on f on f           | `iterate (map (+1)) [1..10]` |

With  "higher-order" functions being functions with order greater than one.

It turns out that the conceptual pattern is useful for type constructors.

| Order | Description           | Kind            | Example   |
|-------|-----------------------|-----------------|-----------|
| 0     | type constant         | `*`             | `Bool`    |
| 1     | type constructor      | `(* -> *)`      | `Maybe a` |
| 2     | function on functions | `(* -> *) -> *` | `HKind a` |

where `HKind a` is, for example:

```haskell
> data HKind a = Z (a Bool)
> :kind HKind
> (* -> *) -> *
> data Id a = Id a
> :type Z (Id True)
Z (Id True) :: HKind Id
```

`HKind`, as well as all type constructors with order greater than 1 are called
"higher-order type constructors" or "higher-kinded types."

Now what is this useful for? There's a really useful typeclass called `Functor`,
that is a higher-kinded type:

```haskell
> :kind Functor
Functor :: (* -> *) -> Constraint
```

But maybe we're getting a little ahead of ourselves here... Functor is covered
in great detail in later chapters.

## 11.5 Data constructors and values

### Exercises: Dog Types

1. type constructor
2. `* -> *`
3. `*`
4. `Num a => Doggies a`
5. `Doggies Integer`
6. `Doggies String`
7. both?
8. `a -> DogueDeBordeaux a`
9. `DogueDeBordeaux String`

## 11.6 What's a type and what's data?

### Exercises: Vehicles

[see `Vehicles.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/11/Vehicles.hs)

## 11.8 What makes these datatypes algebraic?

### Exercises: Cardinality

1. cardinality is 1
2. `3`
3. `2^16 = 65536`
4. `2^64`
5. `Int8` is an 8 bit integer. `2^8` is 256.

### Exercises: For Example

1. `MakeExample`'s type is `Example`, `Example` does not have a type, it is a
   type
2. `Example` has data constructor `MakeExample` with an instance of typeclass
   `Show`
3. `MakeExample` is a function from `Int` to `Example`

## 11.9 newtype

### Exercises: Logic Goats

[see `LogicGoats.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/11/LogicGoats.hs)

## 11.10 Sum types

### Exercises: Pity the Bool

1. 4, `Bool` has cardinality 2 and there are 2 `Bools` in the sum type,
   so `2 + 2 = 4`
2. 258, the type can either be `BoolyBool True`, `BoolyBool False` or a `Numba`.
   If you go over the Int8 bounds, you get a compiler warning
   `-Woverflowedliterals`

When calculating cardinality of types, remember to add sum types and multiply
product types.

## 11.12 Normal Form

## Exercises: How Does Your Garden Grow?

```haskell
data Garden = Gardenia String
            | Daisy String
            | Rose String
            | Lilac String
            deriving Show
```

## 11.13 Constructing and deconstructing values

### Exercises: Programmers

[see `Programmers.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/11/Programmers.hs)

## 11.14 Function type is exponential

Here's how I visualize why the function type is exponential:

The set theory definition is, roughly, that a function is a set of
pairs of elements some input set `A` and elements of some output set `B`,
such that there for each element `a` in `A`, there is one and only one pair
`(a, _)` in `f` (assuming the function is total).

Suppose we're considering functions from `Bool` to `Bool`. As haskell code:

```haskell
f1 :: Bool -> Bool
f1 True = True
f1 False = True
```

But written as a set, `f1` looks like:

```
{ {T, T},  {F, T} }
```

Another function `f2` might be:

```haskell
f2 :: Bool -> Bool
f2 True = True
f2 False = False
```

```
{ {T, T},  {F, F} }
```

So now let's ask ourselves: How many distinct functions from `Bool` to `Bool`
are there?

Well, `Bool` is small so we can just list them out:

```
{ {T, T},  {F, T} }
{ {T, T},  {F, F} }
{ {T, F},  {F, T} }
{ {T, F},  {F, F} }
```

So there are four, which from the book makes sense because `Bool` has a
cardinality of 2 and function types as exponentials implies that
the cardinality of `Bool -> Bool` is `2^2 = 4`.

But, why is this the case? Here's something interesting, in the listing
of possible functions as sets:

```
{ {T, T},  {F, T} }
{ {T, T},  {F, F} }
{ {T, F},  {F, T} }
{ {T, F},  {F, F} }
```

We're actually repeating a lot of information in each line. See how all
the `T`'s and `F`'s line up in the first position of the pairs? We already know
that in each function from `Bool` there's going to be a value from the `True`
input as well as a value from the `False` input. What makes the function unique
is really the outputs, not the inputs.

Let's rewrite the function listing, by picking an order for elements
of `Bool`: True, False.

Then we can rewrite:
```
{ {T, T},  {F, T} } = TT
{ {T, T},  {F, F} } = TF
{ {T, F},  {F, T} } = FT
{ {T, F},  {F, F} } = FF
```

As long as we know the ordering `True, False`, we can figure out that the first
symbol in the pair `TF`, for example, refers to to the output from `True` and
the second symbol refers to the output from `False`.

In other words, when we look at a function, we can look up the function's
output for a given input by looking at what symbol appears the input's position
in the ordering.

For example, what does the function `FT` return for the
input `True`? `FT` has an `F` for `False` in the `True` position, so it
returns `False`.

The function `FT` and `FF` are the same in the `True` position,
and differ in the `False` position.

This may remind you of how digits work in numbers, except instead of
the ones position, tens position etc, the places represent inputs.

Watch what happens if we map the symbol `T` to `1` and the symbol `F` to `0`:

```
TT = 11
TF = 10
FT = 01
FF = 00
```

These are all the 2 digit binary numbers. There are four of them,
because each digit can be either `1` or `0`, and there are two digits,
so `2 symbols ^ 2 digits = 4`

If there were three digits, there would be `2^3 = 8` possible numbers.  If
there were three digits in base ten there would be `10^3 = 1000` possible
numbers.

The elements of a function's input set can be mapped to
"digit" positions, and the elements of the output set can be mapped to "digit"
symbols. Then you can write down a unique representation of the function
by writing the output symbols in the input positions. Because the number of
possible unique representations is the same as the number of possible functions,
and because the number of representations is the number of base symbols raised
to the number of digits (base ^ digitnumber = uniques), the number of possible
unique functions from one set to another is the number elements in the output
set raised to the number of element in the input set.

## Exercises: The Quad

1. 8
2. 16
3. 4^4 = 256
4. 2*2*2 = 8
5. 2^2^2 = 16
6. (4^4)^2 = 65536

## 11.17 Binary Tree

### BinaryTree

[see `BinaryTree.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/11/BinaryTree.hs)

## 11.18 Chapter Exercises

### Multiple Choice

1. a
2. c
3. b
4. c

### Ciphers

[see `Vignere.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/11/Vignere.hs)

### As-patterns

[see `AsPatterns.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/11/AsPatterns.hs)

### Language exercises

[see `LanguageExercises.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/11/LanguageExercises.hs)

### Phone exercise

[see `Phone.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/11/Phone.hs)

[TODO: I'm looking back on this code several months after writing it. It's
awkward, but I'll leave it as is for now, since it's an okay example of solving
the problem with only the tools covered in the book thus far. I think I should
do an example of how this project gets a lot easier when you can use things like
the State monad.]

### Hutton's Razor

[see `Hutton.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/11/Hutton.hs)
