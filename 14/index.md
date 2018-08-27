---
title: "Notes (HPFP 14/31): Testing"
author: jcb
date: 2017-11-01
tags: notes, haskell, hpfp
---

# 14 Testing

## Intermission: Short Exercise

[see `addition/Addition.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/14/addition/Addition.hs)

## 14.7 Chapter Exercises

### Validating numbers into words

[see `exercises/test/Spec.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/14/exercises/test/Spec.hs)

### Using QuickCheck
[see `exercises/test/Spec.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/14/exercises/test/Spec.hs)

### Failure

Irrational numbers like e.g. the sqaure root of 2 cannot be represented
with infinite precision in a finite amount of memory. So an expression
like (sqrt 2) is not actually equal to the square root of 2, but rather
is an accurate approximation to some precision. So even though square
is the inverse of square root, because sqrt cannot be infinitely accurate
the square of a square root will have some error. E.g.

```
sqrt 2 = 1.4142135, (sqrt 2) ^ 2 = 1.9999999
```

### Idempotence

[see `exercises/test/Spec.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/14/exercises/test/Spec.hs)

### Make a Gen random generator for the datatype

[see `exercises/test/Spec.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/14/exercises/test/Spec.hs)

### Hangman testing
~~Skipping this one, I think that this testing chapter is probably better
understood after you understand what monads are. The reader here only barely
has the tools to build something for which testing is important.~~

[see `hangman`](https://github.com/johnchandlerburnham/hpfp/blob/master/14/hangman)

So I went back and did this exercise and my initial instinct has been confirmed.
I found it  a pretty interesting exercise to build a `Gen
Puzzle` arbitrary puzzle generator, but actually applyting that generator to a
reasonable property test for the hangman game is way too complicated for a
beginner given the material covered so far. I figured it out, but it requires
using the `monadicIO` function from the `Test.QuickCheck.Monadic` library,
which is super interesting in how it works, but yeah, definitely something I
only understood on my second pass through this book.

In any case, check out my new and improved hangman. I was able to refactor out a
lot of complexity, using a little monadic goodness. I also simplified the
`Puzzle` type by making it only contain two strings, one for the word and one
for guesses:

```haskell
data Puzzle = Puzzle { word :: String, guessed :: String } deriving Show
```

This simplifies the code enormously, because instead of dragging around a
stateful `filledIn :: [Maybe Char]`, we can instead just compute:

```haskell
discovered :: Puzzle -> [Maybe Char]
discovered p = (\x -> if elem x (guessed p) then Just x else Nothing) <$> word p
```

This is a little slower, sure, to recompute `discovered` every time we want to
check the characters that have already been solved rather than already having it
in our `Puzzle` type. But honestly, the scope of the game is so small that it
doesn't matter. And if for some reason we need `Puzzle` to work efficiently with
million-character words, there are better ways to cache that computation, and
bigger inefficiencies, like the use of `String`, that we would have to address
first.

### Validating ciphers

[see `ciphers`](https://github.com/johnchandlerburnham/hpfp/blob/master/14/ciphers)

This uses a little `newtype` trick to make sure that the arbitrary strings we
generate are all composed of lowercase characters. The reason being that the
`vignere` and `caesar` functions have a built-in filter that ensures they only
work on lowercase characters. Arguably not a great design decision on my part
when I wrote `vignere` and `caesar`, but I wanted to test the functions I
actually wrote in the previous chapters rather than rework them.

## 14.9 Follow-up resources

1. [Pedro Vasconcelos; An introduction to QuickCheck testing;](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing)
2. Koen Claessen and John Hughes; (2000)
QuickCheck: A Lightweight Tool for Random Testing of Haskell
Programs
3. [Pedro Vasconcelos;Verifying a Simple Compiler Using
Property-based Random Testing;](
http://www.dcc.fc.up.pt/dcc/Pubs/TReports/TR13/dcc-2013-06.pdf)

---
