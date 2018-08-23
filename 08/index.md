# 8 Recursion

By the way, the Borges quote at the start of this chapter is only an example
of recursion if the cartographer making the perfect map of England is himself in
England. If instead, the cartographer is in Argentina (but not, perhaps Las
Islas Malvinas) we don't have self-reference, just reference.

## 8.1 Recursion

For a thorough work-through of the fixed point combinator, see my [notes on
Rojas' Tutorial Introduction to Lambda Calculus.](/projects/tilc/04)


## Intermission: Exercise

```
applyTimes 5 (+1) 5
```
turns  into

```
((+1) . (+1) . (+1) . (+1) . (+1)) 5
```

## 8.3 Bottom

The bottom value in Haskell is what you get when you try to compute something
that doesn't compute. It's a crash, an exception, an infinite loop and a fly in
the ointment. We can't totally get rid of the possibility of bottom in our code
without sacrificing a lot of useful expressive power ([because of the halting
problem](https://en.wikipedia.org/wiki/Halting_problem)).

Bottom is a reminder that we don't actually right code in the Platonic realm of
pure Forms, but in this messy, maddening and often bafflingly broken world we
call home. Sometimes with Haskell I find it easy to forget this, because the
math is just so pretty. Other times I run across bottom-generating things like
partial functions, runtime exceptions, and `unsafeCoerce`, and [I
remember](https://hackage.haskell.org/package/base-4.11.1.0/docs/Unsafe-Coerce.html).

## 8.6 Chapter Exercises

### Review of types

1. d
2. b
3. d
4. b

### Reviewing currying

see `ReviewingCurrying.hs`

1. `"woops mrow woohoo"`
2. `"1 mrow haha"`
3. `"woops mrow 2 mrow haha"`
4. `"woops mrow blue mrow haha"`
5. `"pink mrow haha mrow green mrow woops mrow blue"`
6. `"are mrow Pugs mrow awesome"`

### Recursion

1.
    ```
    dividedBy 15 2 ->
    go 15 2 0 ->
    go 13 2 1 ->
    go 11 2 2 ->
    go 9 2 3 ->
    go 7 2 4 ->
    go 5 2 5 ->
    go 3 2 6 ->
    go 1 2 7 ->
    (7, 1)
    ```

2.  see `RecursiveSum.hs`

3.  see `RecursiveMult.hs`

### Fixing dividedBy:

see `DividedBy.hs`

### McCarthy 91 function:

see `McCarthy91.hs`

### Numbers into Words:

see `WordNumber.hs`

---
