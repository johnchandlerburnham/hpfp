---
title: "Notes: Haskell Programming from First Principles by Chris Allen & Julie
Moronuki"
---

![](/images/hpfp-cover.png)

# Contents

- [Chapter 01: All You Need is Lambda](/projects/hpfp/01)
- [Chapter 02: Hello, Haskell!](/projects/hpfp/02)
- [Chapter 03: Strings](/projects/hpfp/03)
- [Chapter 04: Basic Datatypes](/projects/hpfp/04)
- [Chapter 05: Types](/projects/hpfp/05)
- [Chapter 06: Typeclasses](/projects/hpfp/06)
- [Chapter 07: More functional patterns](/projects/hpfp/07)
- [Chapter 08: Recursion](/projects/hpfp/08)
- [Chapter 09: Lists](/projects/hpfp/09)
- [Chapter 10: Folding lists](/projects/hpfp/10)
- [Chapter 11: Algebraic datatypes](/projects/hpfp/11)
- [Chapter 12: Signaling adversity](/projects/hpfp/12)
- [Chapter 13: Building Projects](/projects/hpfp/13)
- [Chapter 14: Testing](/projects/hpfp/14)
- [Chapter 15: Monoid, Semigroup](/projects/hpfp/15)
- [Chapter 16: Functor](/projects/hpfp/16)
- [Chapter 17: Applicative](/projects/hpfp/17)
- [Chapter 18: Monad](/projects/hpfp/18)
- [Chapter 19: Applying Structure](/projects/hpfp/19)
- [Chapter 20: Foldable](/projects/hpfp/20)
- [Chapter 21: Traversable](/projects/hpfp/21)
- [Chapter 22: Reader](/projects/hpfp/22)
- [Chapter 23: State](/projects/hpfp/23)
- [Chapter 24: Parser Combinators](/projects/hpfp/24)
- [Chapter 25: Composing type](/projects/hpfp/25)
- [Chapter 26: Monad transformers](/projects/hpfp/26)
- [Chapter 27: Nonstrictness](/projects/hpfp/27)
- [Chapter 28: Basic libraries](/projects/hpfp/28)
- [Chapter 29: IO](/projects/hpfp/29)
- [Chapter 30: When things go wrong](/projects/hpfp/30)
- [Chapter 31: Final project](/projects/hpfp/31)

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

Lastly, my GitHub repository containing my solutions for HPFP is
[here](https://github.com/johnchandlerburnham/haskellbook).

