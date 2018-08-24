---
title: "Notes (HPFP 10/31): Folding Lists"
author: jcb
date: 2017-11-01
tags: hpfp, notes, haskell
---

# 10 Folding lists

Okay, so here's the thing about the term "catamorphism":

"Kata" in Greek means "down". The opposite of "kata" is "ana" which means "up".

So we have "catamorphisms" and "anamorphisms". Remember that "morph" means
"form", so a "catamorphism" is a "down-form thing" and an "anamorphism"
is an "up-form thing".

But what the heck do "up" and "down" have to do with "forms". There's a metaphor
that recurs (so to speak) again and again in functional programming between
height and complexity: Things that have more structure are upwards and things
that have less structure are downwards. It's like a tall building: the more
structure you have the higher you go.

So an `Integer` is pretty simple, and is downwards of `[Integer]` or `Maybe
Integer` or `Map String Integer`.

Functions that go "upwards" in this complexity-space, like from `Integer ->
[Integer]` are, roughly speaking, anamorphisms. Functions that go "downwards"
are catamorphisms.


## 10.4 Fold right

### Exercises: Understanding folds

[see `Folds.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/10/Folds.hs)

### Exercises: Database Processing

[see `DatabaseProcessing.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/10/DatabaseProcessing.hs)

## 10.9 Scans

### Scans Exercises

[see `Scans.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/10/Scans.hs)

## 10.10 Chapter Exercises

### Warm-up and reveiw

[see `WarmUp.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/10/WarmUp.hs)

### Rewriting functions using folds

[see `FunctionsUsingFolds.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/10/FunctionsUsingFolds.hs)

## 10.12 Follow-up resources

1. [Haskell Wiki. Fold.](https://wiki.haskell.org/Fold)

2. [Richard Bird. Sections 4.5 and 4.6 of Introduction to Functional](Programming using Haskell (1998).)

3. Antoni Diller. Introduction to Haskell.

4. [Graham Hutton. A tutorial on the universality and expressive-
ness of fold.](http://www.cs.nott.ac.uk/~gmh/fold.pdf)

---
