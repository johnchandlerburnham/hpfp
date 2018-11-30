---
title: "Notes (HPFP 29/31): IO"
---

# 29 IO

One interesting thing about how `IO` is defined is that

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

is that it inverts the usual perspective of subjective thought happening within
an objective world. `IO` is like looking into the universe from
the plane of pure Platonic computation, calculating its physics forward a step
in time, and then plucking out the type `a` you happen to want. It's a very
heady perspective, and small wonder people get hooked on it.

But it's important to remember that this perspective is a lie. GHC is only
simulating the outside-view; the code we run is still very much within the
universe and generating causal effects like heating our cpu, increasing our
electric bill etc. etc. These effects are usually (but not always) small, which
is what makes the simulated outside perspective useful.

The outside perspective preserves purity by modeling (or pretending to model)
the whole universe as a pure function. The `IO` type is that model, it makes
`GHC` safely switch off simulating computation as something that happens outside
of time and space. Within `IO`, `GHC` respects RealWorld time, and doesn't mess
with our execution order.

There is also an unsafe way to punch a hole in the simulation:

```haskell
import System.IO.Unsafe

unsafe :: Int
unsafe = unsafePerformIO (print "a hole in the simulation" >> return 2)

main :: IO ()
main = do
  putStrLn "the outside perspective"
  print $ 2 + unsafe
```

## 29.9 Chapter Exercises

### File IO with Vigenere

see `src/VignereFile.hs`

### Config directories

see `src/ConfigDirectories.hs`

I love how terse this program is. Just traverse a parser and zip!

## 29.10 Follow-up resources
1. [Referential Transparency; Haskell Wiki](https://wiki.haskell.org/Referential_transparency)
2. [IO Inside; Haskell Wiki](https://wiki.haskell.org/IO_inside)
3. Unraveling the mystery of the IO Monad; Edward Z. Yang
4. [Primitive Haskell; Michael Snoyman](https://github.com/commercialhaskell/haskelldocumentation/blob/master/content/primitive-haskell.md)
5. [Evaluation order and state tokens; Michael Snoyman](https://wiki.haskell.org/Evaluation_order_and_state_tokens)
6. Haskell GHC Illustrated; Takenobu Tani
7. [Tackling the Awkward Squad; Simon Peyto Jones](http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/mark.pdf)
8. Note [IO hack in the demand analyser]; GHC source code
9. Monadic I/O in Haskell 1.3; Andrew D. Gordon and Kevin Ham-
mond
10. [Notions of computation and monads; Eugenio Moggi](http://www.disi.unige.it/person/MoggiE/ftp/ic91.pdf)
11. The Next 700 Programming Languages; P. J. Landin




