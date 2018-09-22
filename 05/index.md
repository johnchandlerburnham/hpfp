---
title: "Notes (HPFP 05/31): Types"
author: jcb
date: 2017-11-01
---

# 5 Types

The quote at the beginning of this chapter is an excerpt from the Wallace
Stevens poem, [The Idea of Order at Key
West](https://www.poetryfoundation.org/poems/43431/the-idea-of-order-at-key-west).
The whole poem resonates with me; it aptly captures the aesthetics of type level
programming.

## 5.3 How to read type signatures

The type constructor for function `(->)` isn't magic. It's exactly like any
other type constructor. Recall how previously with lists the type constructor
was `[]`:

```haskell
Prelude> :i []
data [] a = [] | a : [a]  -- Defined in ‘GHC.Types’
```

There isn't any reason other than cleaner syntax that `[] a` or `[a]` couldn't
be `List a`.

Well the function type is almost exactly the same. There isn't any reason
why `a -> b`, which is `(->) a b`, couldn't be `Fun a b`. It's just more
syntactic sugar (Haskell is a very sugary language. That's why it's so sweet!)

```haskell
Prelude> :i (->)
data (->) t1 t2   -- Defined in ‘GHC.Prim’
infixr 0 `(->)`
```

There isn't any definition for the function type though because it's a
primitive (hence the `GHC.Prim`).

### Exercises: Type Matching

1.  a. `not :: Bool -> Bool`
    b. `length :: [a] -> Int`
    c. `concat :: [[a]] -> [a]`
    d. `head :: [a] -> a`
    e. `(<) :: Ord a => a -> a -> Bool`


## 5.4 Currying

All these types for `f` are equivalent:

```haskell
type Fun = (->)
f :: a -> a -> a
f :: a -> (a -> a)
f :: (->) a ((->) a a)
f :: Fun a (Fun a a)
```

Functions in Haskell return one and only one thing. Partial application
is kind of a silly term. What's partial about applying a `Fun a (Fun a a)` to
an `a`? You give it an `a`, it gives you a function. Nothing partial about
that. Maybe we wanted a function.

It's only partial if you think of functions of somehow not being final values.
Which is how they are in imperative-land. But we're not in imperative-land
anymore. Here functions are first-class, so they actually are values like
anything else.

In fact, if you remember any of Chapter 1, there's a good argument to be made
that functions are more real than any other value. Lambda calculus builds
the whole universe out of functions. Sure literals exist, but they're
a convenience (a huge convenience) not a strict necessity.

The `curry` and `uncurry` functions in the text are useful to understand
conceptually.

Sectioning is basically bad practice. It's a great way to confuse yourself
and others. Do yourself a favor and throw an abstraction on top of it:

Not
```
y = (2^)
z = (^2)
```

But rather,
```
y = \x -> 2^x
z = \x -> x^2
```

You see how much nicer that is? Don't abuse infix operators please. They're
there to make text more legible, not more terse and inscrutable. There are
definitely cases where cleverly sectioning an infix operator can make things
clearer, but in my opinion, these cases are exceptions.

### Exercises: Type Arguments

1. a
2. d
3. d
4. c
5. a
6. e
7. e
8. e
9. c

## 5.5 Polymorphism

Greek words are everywheer in Haskell-land. Greek and Latin, especially in the
sciences, act like a kind of conceptual assembly language that allows you to
build words for complex ideas out of concrete concepts. But if you don't know
the roots, the vivid metaphor embedded in the word becomes murky and unclear. It
would be very confusing if we used English for this, because then the same word
would be used for both substantive and metaphoric meaning.

Let's take the word "Polymorphism" for example. This breaks apart into three
Greek roots:

- "poly-", from the Greek "πολύς", meaning "many".
- "-morph-", from the Greek "μορφή", meaning "shape, form, type"
- "-ism", from the Greek "-ισμός", which is a suffix forming abstract nouns.

So the word means literally "The abstract property of having many forms or
types." Or, maybe more tersely: "many-formed-ness".

Some important Greek roots:

```
hyle: matter
morphe: form

polys: - many
monos: - one

autos: self
endon: in
ectos: out
isos: equal

ana: up
kata: down
epi:  upon
meta: beyond, with
para: beside
meter: measure
```

So when you read "parametric polymorphism", fear not, you are really
reading "beside-measure many-form-thing." The latter doesn't sound nearly
as clever at cocktail parties, but that's actually what the words mean,
and knowing the meanings of the words you use helps you remember the
concepts they describe.

A parameter is quite literally a "side measure." When we measure
a thing by looking at it next to something else, we're using a parameter.
Ever ask whether something was bigger than a breadbox? That's measuring
size in terms of breadboxes. It's a side measure. It's a parameter.

The function `id :: a -> a` is called parametrically polymorphic, because it's
using `a` as the side-measure, or parameter.

This is a little clearer if we enabled the `ExplicitForall` language extension:

```haskell
{-# LANGUAGE ExplicitForAll #-}
module SideMeasure where

id' :: forall a. a -> a
id' a = a
```

The `forall` is actually implicit in the regular type definition of `id`, but
we've just made it explicit here. All it's saying is that whatever `a` type the
argument of `id'` has, `id'` will always return something of the same type.

Interestingly this implies that the only thing we can do with `id'` is just
return the argument unchanged. You can see this pretty clearly, I think, if you
consider what has to happen if you pass in a type that only has a single value
inhabiting it (like `()`, the Unit type).

Constrained polymorphism, by the way, is where we restrict that `forall`, to
just a `forall` within some class of types (a "typeclass", if you will). So a
function like `negate :: Num a => a -> a`, only makes the same "for all"
commitment for types within the `Num` typeclass. The `Num a =>` is called a
constraint, or typeclass constraint, by the way.

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

## 5.6 Type Inference

Type inference is a cool tool for helping us build better programs. But
it works best when you give it annotations to infer from. A lot of Haskell
programming involves defining the types of the top-level expressions in your
program before you actually start constructing anything, so this isn't exactly
any extra work.

And if you want to see real type system magic at work: [Typing the technical
interview](https://aphyr.com/posts/342-typing-the-technical-interview)


### Exercises: Apply Yourself

1. `[Char] -> [Char]`
2. `Fractional a => a -> a`
3. `Int -> [Char]`
4. `Int -> Bool`
5. `Char -> Bool`

## 5.8 Chapter Exercises

### Multiple Choice
1. c
2. a
3. b
4. c

### Determine the type:

1.  a. `Num a => a`
    b. `Num a => (a, [Char])`
    c. `(Integer, [Char])`
    d. `Bool`
    e. `Int`
    f. `Bool`

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

1. [see Sing.hs](https://github.com/johnchandlerburnham/hpfp/blob/master/05/Sing.hs)
2. [see Sing.hs](https://github.com/johnchandlerburnham/hpfp/blob/master/05/Sing.hs)
3. [see Arith3Broken.hs](https://github.com/johnchandlerburnham/hpfp/blob/master/05/Arith3Broken.hs)

### Type-Kwon-Do

1. `h x = g $ f x`
2. `e x = w $ q x`
3. `xform (x, y) = (xz x, yz y) `
4. `munge f g x = fst $ g $ f x`


## 5.10 Follow-up resources

1. [Luis Damas; Robin Milner. Principal type-schemes for functional
   programs](https://github.com/johnchandlerburnham/hpffp-resources/blob/master/Chapter-05/Principal%20type-schemes%20for%20functional%20programs.pdf)

2. [Christopher Strachey. Fundamental Concepts in Programming
Languages](https://github.com/johnchandlerburnham/hpffp-resources/blob/master/Chapter-05/Fundamental%20Concepts%20in%20Programming%20Languages.pdf)
Popular origin of the parametric/ad-hoc polymorphism distinction.

---
