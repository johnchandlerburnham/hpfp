---
title: "Notes (HPFP 28/31): Basic Libraries
---

# 28 Basic Libraries

## 28.2 Benchmarking with Criterion

[see `benchmark/src/Index.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/28/benchmark/src/Index.hs)

## 28.3 Profiling your programs

[see `benchmark/profilingTime.prof`](https://github.com/johnchandlerburnham/hpfp/blob/master/28/benchmark/profilingTime.prof)
[see `benchmark/src/profilingSpace.ps`](https://github.com/johnchandlerburnham/hpfp/blob/master/28/benchmark/profilingSpace.ps)

## 28.4 Constant applicative forms

[see `benchmark/cafSaturation.prof`](https://github.com/johnchandlerburnham/hpfp/blob/master/28/benchmark/cafSaturation.prof)
[see `benchmark/cafSaturation2.prof`](https://github.com/johnchandlerburnham/hpfp/blob/master/28/benchmark/cafSaturation2.prof)
[see `benchmark/cafSaturation3.prof`](https://github.com/johnchandlerburnham/hpfp/blob/master/28/benchmark/cafSaturation3.prof)

## 28.5 Map

### Exercise: Benchmark Practice

[see `benchmark/src/mapSetBench.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/28/benchmark/src/mapSetBench.hs)

## 28.8 Vector


### Exercises: Vector

Enable profiling in `stack.yaml` with:

```
build:
  library-profiling: true
  executable-profiling: true
```

[see `benchmark/src/vectorMemory.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/28/benchmark/src/vectorMemory.hs)

## 28.10 Chapter Exercises

### Difference List

[see `benchmark/src/DList.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/28/benchmark/src/DList.hs)

### A simple queue

[see `benchmark/src/Queue.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/28/benchmark/src/Queue.hs)

## 28.10 Chapter Exercises

## 28.11 Follow-up resources
1. [Criterion tutorial; Bryan O’Sullivan](http://www.serpentine.com/criterion/tutorial.html)
2. [Demystifying DList; Tom Ellis](http://h2.jaguarpaw.co.uk/posts/demystifying-dlist/)
3. [Memory Management; GHC; Haskell Wiki](https://wiki.haskell.org/GHC/Memory_Management)
4. [Performance; Haskell Wiki](https://wiki.haskell.org/Performance)
5. Pragmas, specifically UNPACK; GHC Documentation
6. [High Performance Haskell; Johan Tibell](http://johantibell.com/files/slides.pdf)
7. Haskell Performance Patterns; Johan Tibell
8. Faster persistent data structures through hashing; Johan Tibell
9. Lazy Functional State Threads; John Launchbury and Simon
Peyton Jones
10. Write Haskell as fast as C: exploiting strictness, laziness and
recursion; Don Stewart
11. Haskell as fast as C: A case study; Jan Stolarek
12. Haskell FFT 11: Optimisation Part 1; Ian Ross
13. Understanding the RealWorld; Edsko de Vries
14. [Stream Fusion; Duncan Coutts](http://code.haskell.org/~dons/papers/icfp088-coutts.pdf)
15. Purely functional data structures; Chris Okasaki
