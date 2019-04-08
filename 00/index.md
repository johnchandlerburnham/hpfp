---
title: "Notes: Haskell Programming from First Principles by Chris Allen & Julie
Moronuki"
author: jcb
date: 2017-11-01
tags: hpfp, notes, haskell
---

![](/images/hpfp-cover.png)

# Contents

- [Chapter 01: All You Need is Lambda](/01)
- [Chapter 02: Hello, Haskell!](/02)
- [Chapter 03: Strings](/03)
- [Chapter 04: Basic Datatypes](/04)
- [Chapter 05: Types](/05)
- [Chapter 06: Typeclasses](/06)
- [Chapter 07: More functional patterns](/07)
- [Chapter 08: Recursion](/08)
- [Chapter 09: Lists](/09)
- [Chapter 10: Folding lists](/10)
- [Chapter 11: Algebraic datatypes](/11)
- [Chapter 12: Signaling adversity](/12)
- [Chapter 13: Building Projects](/13)
- [Chapter 14: Testing](/14)
- [Chapter 15: Monoid, Semigroup](/15)
- [Chapter 16: Functor](/16)
- [Chapter 17: Applicative](/17)
- [Chapter 18: Monad](/18)
- [Chapter 19: Applying Structure](/19)
- [Chapter 20: Foldable](/20)
- [Chapter 21: Traversable](/21)
- [Chapter 22: Reader](/22)
- [Chapter 23: State](/23)
- [Chapter 24: Parser Combinators](/24)
- [Chapter 25: Composing type](/25)
- [Chapter 26: Monad transformers](/26)
- [Chapter 27: Nonstrictness](/27)
- [Chapter 28: Basic libraries](/28)
- [Chapter 29: IO](/29)
- [Chapter 30: When things go wrong](/30)
- [Chapter 31: Final project](/31)

# Preliminary Remarks

This page contains my notes and exercise solutions for the text
"Haskell Programming From First Principles" by Chris Allen and Julie Moronuki.

Brief review: I love this book. I love Haskell. I worry this book has been so
good at teaching me Haskell that I'm going to love other languages less by
comparison. The book's philosophy of "Let's break complicated topics down into
simple pieces, and then play with the pieces until they become obvious" is so
effective that I'm going to just reflexively hold every other CS book to the
same standard from now on. I suspect this is going to leave me very disappointed,
since most other programming books aren't anywhere near this fun.

Okay, maybe that's a little hyperbolic. I don't want to give the impression
that HPFP is a text without flaws. I'll probably talk about those flaws at some
point, but before I do I want to make it perfectly clear that this book is
spectacular and any criticism is made with the greatest possible love
and affection.

I deeply admire what Julie Moronuki and Chris Allen have accomplished here, and
I encourage anyone interested in programming to buy a copy at
[www.haskellbook.com](www.haskellbook.com). I am also eagerly awaiting
Julie's upcoming book [The Joy of Haskell](https://joyofhaskell.com/) and
Chris' [Haskell Almanac](https://lorepub.com/product/cookbook).

A little house-keeping first: My notes and exercise solutions, are not designed
to make much sense without reference to the text itself. My intended reader is
someone who has already bought HPFP and wants to check their own work
against someone else's. Please buy the book; supporting great authors now means
more great books for everyone in the future.

Also, each chapter of HPFP has a list follow-up resources at the end. In my
chapter notes, I'll link to separate posts containing my notes on each of these
resources.

