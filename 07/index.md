---
title: "Notes (HPFP 07/31): More functional patterns"
author: jcb
date: 2017-11-01
---

# 7 More functional patterns

## 7.1 Make it func-y

Functions aren't just transformations of values; functions are themselves values
that can be used, manipulated and transformed like other values.

## 7.2 Arguments and parameters


There's a subtle distinction between an *argument* to a function and a
*parameter* of a function.

Let's consider the function

```haskell
increment :: Int -> Int
increment x = x + 1
```

and when we apply it:

```haskell
> increment 1
2
```

The `x` in `increment x` is a parameter. The `1` in `increment 1` is an
argument. Parameters are like variables that represent arguments, whereas
arguments are actual values that are passed in to the function.

In a way, parameters are potential arguments, and arguments are actualized
parameters. In common language however, sometimes the distinction between act
and potency - whether something really is a thing, or whether it merely could
become that thing - is often very murky, so sometimes you'll read people using
the word *parameter* where they really mean *argument* and vice-versa.

For a clearer understanding of the distinction between act and potency, I
recommend this [post by Ed
Feser](https://edwardfeser.blogspot.com/2009/05/act-and-potency.html) and this
chapter from [Reginald Garrigou-Lagrange’s Reality: A Synthesis of Thomistic
Thought](http://www.thesumma.info/reality/reality6.php).

The concepts of *scoping* and *name-shadowing* are best understood in the
context of the lambda calculus. I recommend reading at least chapter 1 of my
[notes on Rojas' Introduction to the Lambda
Calculus](/posts/workthrough-lambda-calculus-rojas.html#definition).

## 7.3 Anonymous functions

Anonymous functions are lambda expressions. The following are equivalent:

```haskell
anon y = y
anon = \y -> y
```

Why are anonymous functions useful? Because they're more fundamental! The name
of a function is an extra detail, that sometimes we want and sometimes we don't.

Take the two functions:

```haskell
f x = x
g x = x
```

Are `f` and `g` the same function? Yes and no! Yes, because they do the same
thing, but no, because the have different names!

We can make the distinction explicit by writing out the anonymous function that
`f` and `g` wrap their names around:

```haskell
f = \x -> x
g = \x -> x
```

### Exercises: Grab bag

1.  They are all equivalent
2.  c, because `mTh 3` is like `\y z -> 3 * y * z`
3.  a. `f = \x -> x + 1`
    b. `addFive = \x -> \y -> (min x y) + 5`
    c. `mflip f x y = f y x`

## 7.4 Pattern matching

Pattern matching is just case expressions under the hood:

From [Gonzalez's "How to Desugar Haskell Code"](https://github.com/johnchandlerburnham/hpffp-resources/blob/master/Chapter-02/How%20to%20desugar%20Haskell%20code.pdf)

> Pattern matching on constructors desugars to case statements:
>
> ```haskell
> f (Left  l) = eL
> f (Right r) = eR
>
> -- ... desugars to:
>
> f x = case x of
>     Left  l -> eL
>     Right r -> eR
> ```
>
> Pattern matching on numeric or string literals desugars to equality tests:
>
> ```haskell
> f 0 = e0
> f _ = e1
>
> -- ... desugars to:
> f x = if x == 0 then e0 else e1
>
> -- ... desugars to:
> f x = case x == 0 of
>     True  -> e0
>     False -> e1
> ```



### Exercises: Variety Pack

[see `VarietyPack.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/07/VarietyPack.hs)

One important thing to remember about tuple syntax in Haskell is that the type
constructor and data constructor syntax for tuples is exactly the same, and has
the same syntactic sugar that makes `(a,b)` just a prettified `(,) a b`:

```haskell
> :i (,)
data (,) a b = (,) a b
> (,) 1 2
(1,2)
> :t (,) 1 2
(,) 1 2 :: (Num b, Num a) => (a, b)
```

Also, the different sizes of n-tuples are defined "by hand" in GHC essentially
independently from one another. There isn't some complicate meta n-tuple
generating logic; they're literally just defined as separate types.

```
> :i (,,)
data (,,) a b c = (,,) a b c
> (,) 1 2 3
(1,2,3)
> :i (,,,)
data (,,,) a b c d = (,,,) a b c d
```

Seriously, take a look at [the
source](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Tuple.html)

In fact, if we try to write a 63-tuple, we get the following error:

```haskell
> :t (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)

<interactive>:1:1: error:
    A 63-tuple is too large for GHC
      (max size is 62)
      Workaround: use nested tuples or define a data type
```

One of the amusing idiosyncracies of GHC.

## 7.5 Case expressions

Pattern matching, if-then-else, guards: they all desugar to case expressions.

### Exercises: Case Practice

see `CasePractice.hs`

## 7.6 Higher-order functions

The reason why a function that acts on other functions is called a "higher-order
function" is that expressions can be organized by how many layers of functions
there are.

| Order | Description           | Example                      |
|-------|-----------------------|------------------------------|
| 0     | value, or literal     | "foobar", `3`                |
| 1     | function              | `((+) 3 4)`                  |
| 2     | function on functions | `map (+1) [1..10]`           |
| 3     | f on f on f           | `iterate (map (+1)) [1..10]` |

It turns out though, that all expression with order two or greater are
essentially equivalent (especially if we have currying), so we can really just
collapse the above chart into *values*, *first-order functions*, and
*higher-order functions*.

### Exercises: Artful Dodgy

2. `11`
3. `22`
4. `21`
5. `12`
6. `11`
7. `21`
8. `21`
9. `22`
10. `31`
11. `23`

## 7.7 Guards

### Exercises: Guard Duty

[see `AvgGrade.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/07/AvgGrade.hs)

1. Can't do otherwise if theres no wise to other.
2. No, because the conditions are not exclusive.
3. b
4. anything reversible, so lists
5. `[a] -> Bool`
6. c
7. `(Ord a, Num a) => a`
8. `(Ord a, Num a) => a -> Bool`

## 7.8 Function Composition

The important thing to remember about composing functions is that they're a lot
like Legos; the shapes have to fit together. So when you compose functions `f`
and `g` as `f . g`, the output of `g` has to fit the input of `f`.

## 7.9 Pointfree style

Sometimes writing a pointfree style function is easier to read, sometimes its
not. In the vast majority of cases, code will behave exactly the same whether
it's pointfree or "point-full." GHC is super smart and usually doesn't really
care about this kind of stylistic detail.

The primary consideration of writing pointfree code should be whether it is more
legible to another human being. Pointfree style code is easily abused to create
impenetrable code. Think about whether a good parameter name might make it
easier for another person to understand the function you're writing.

## 7.11 Chapter Exercises

### Multiple Choice

1. d
2. b
3. d
4. b
5. a

### Let's write code

[see `LetsWriteCode.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/07/LetsWriteCode.hs)
[see `Arith4.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/07/Arith4.hs)


## 7.13 Follow-up resources

1. [Paul Hudak; John Peterson; Joseph Fasel. A Gentle Introduction
to Haskell, chapter on case expressions and pattern matching.](https://github.com/johnchandlerburnham/hpffp-resources/blob/master/Chapter-07/Case%20Expressions%20and%20Pattern%20Matching.pdf)
2. [Simon Peyton Jones. The Implementation of Functional Programming Languages, pages
53-103.](https://github.com/johnchandlerburnham/hpffp-resources/blob/master/Chapter-07/The%20Implementation%20of%20Functional%20Programming%20Languages.pdf)
3. [Christopher Strachey. Fundamental Concepts in Programming
Languages, page 11 for explanation of
currying.](https://github.com/johnchandlerburnham/hpffp-resources/blob/master/Chapter-07/Fundamental%20Concepts%20in%20Programming%20Languages.pdf)
4. [J.N. Oliveira. An introduction to pointfree programming.](http://www.di.uminho.pt/~jno/ps/iscalc_1.ps.gz)
5. [Manuel Alcino Pereira da Cunha. Point-free Program
   Calculation.](https://github.com/johnchandlerburnham/hpffp-resources/blob/master/Chapter-07/Point-free%20Program%20Calculation.pdf)

---
