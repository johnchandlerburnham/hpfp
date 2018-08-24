---
title: "Notes (HPFP 13/31): Building Projects"
author: jcb
date: 2017-11-01
tags: notes, haskell, hpfp
---

# 13 Building Projects

## 13.6 More on importing modules

## Imtermission: Check your understanding

1. `forever`, `when`
2. `Data.Bits`, `Database.Blacktip.Types`
3. Types for `blacktip`'s database
4.
- a. `MV` is `Control.Concurrent.MVar`, `FPC` is `Filesystem.Path.CurrentOS`,
  `CC` is `Control.Concurrent`
- b. `Filesystem`
- c. `Control.Monad`

## Chapter Exercises

### Hangman game logic

See my [hangman project on GitHub](
https://github.com/johnchandlerburnham/hpfp/tree/master/13/hangman)


~~I debated whether or not to include code snippets of full-fledged stack
projects in this document. I've decided against it. It's one thing to include
self-contained modules as question answers, but if I were to, for example,
include the `Main.hs` file for the hangman project here, there'd be some
implicit dependencies like dict.txt and hangman.cabal, and if the past 524
pages of this book have taught me anything, implicit dependencies are bad
news.~~

Upon further reflection, I've decided to axe essentially all code snippets from
my notes. Code is much better in a type-checked source file than in a markdown
file where it can diverge from it's reference. I may add the snippets back if I
figure out a clever way to get `hakyll` to auto-include them. Or I might add
links to the files on Github. TBD.

### Modifying code

1. [see `CipherIO.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/13/CipherIO.hs)
2. [see `ExitSuccess.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/13/ExitSuccess.hs)
3. [see `ExitSuccess.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/13/ExitSuccess.hs)
4. [see `Person.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/13/Person.hs)

## 13.15 Follow-up resources

1. [Stack](https://github.com/commercialhaskell/stack)
2. [How I Start: Haskell](http://bitemyapp.com/posts/2014-11-18-how-i-start-haskell.html)
3. [Cabal FAQ](https://www.haskell.org/cabal/FAQ.html)
4. [Cabal user’s guide](https://www.haskell.org/cabal/users-guide/)
5. [A Gentle Introduction to Haskell, Modules chapter.](https://www.haskell.org/tutorial/modules.html)
