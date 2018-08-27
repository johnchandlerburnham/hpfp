---
title: "Notes (HPFP 16/31): Functor"
author: jcb
date: 2017-11-01
tags: notes, haskell, hpfp
---
# 16  Functor

## 16.4 Let's talk about 4, baby

### Excercises: Be Kind

1. `*`
2. `b` is `* -> *`, `T` is `* -> *`
3. `* -> * -> *`

## 16.7 Commonly used functors

### Exercises: Heavy Lifting
[see `HeavyLifting.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/16/HeavyLifting.hs)

## 16.10 Exercises: Instances of Func

[see `FunctorInstances.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/16/exercises/src/FunctorInstances.hs)

* 8. Trivial doesn't have anything inside it that fmap can apply a function to,
   fmap doesn't make sense for things with kind `*`, or rather fmap on type
   constants is just function application, which is all f and no map.

## 16.11 Ignoring possibilities

### Exercise: Possibly

[see `PossiblyEither.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/16/PossiblyEither.hs)

### Short Exercise

2.  The `a` in `First a` might be a different type than the `b` in
   `Second b`. The function we pass to fmap can only operate on one of those
types, but not both. In other words the function that fmap maps is of type `b
-> c` and kind `*`. Furthermore, we have to apply the function to `Second b`
rather than `First a`, because the structure that fmaps maps onto is of kind `*
-> *`. Our structure is `(Sum a)` because `Sum` is of kind `* -> * -> *` and
it needs to have accepted every type constructer but the last before its
something that fmap can work on. But that doesn't mean we're barred from
writing another function that does something different to `Sum`, but something
different wont be fmap.


## 16.7 Chapter Exercises

Determine if a valid Fucntor can be written for the datatype provided:

1. No, Bool has kind `*` but fmap only works on `* -> *`
2. Yes, note that `False'` and `True'` both take the same type `a`
3. Yes, fmap can ignore `Falsish`
4. Yes... but why...
5. Nope, theres nothing to fmap over, kind `*`

Rearrange the arguments:

[see `Rearrange.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/16/Rearrange.hs)

## 16.19 Follow-up resources

1. [Haskell Wikibook; The Functor class.](https://en.wikibooks.org/wiki/Haskell/The_Functor_class)
2. Mark P. Jones; A system of constructor classes: overloading and implicit higher-order polymorphism.
3. Gabriel Gonzalez; The functor design pattern.

---
