# 27 Nonstrictness

## 27.1 Laziness

Call-by-need and call-by-name are part of a much larger family of evaluation
strategies. The "call" in "call-by", refers to what happens when you "call" a
function, and how that function-call handles it's arguments. Some common
evaluation strategies are:

- call-by-value (strict): Arguments get evaluated first, if the arguments contain
  function calls, then the arguments of those functions get evaluated first.
- call-by-reference (strict): Like call by value, but the arguments that get
  passed to functions aren't expressions, but references to mutable expressions
  (like a pointer). This means that if the same argument gets passed to a
  funtion multiple times, it will only get evaluated once, because the mutable
  expression in the reference only gets evaluated the first time.
- call-by-name (lazy): Arguments are expressions that get evaluated first and then
  directly substituted into the function body. So all functions get transformed
  into nullary functions before they get evaluated.
- call-by-need (lazy) : Like call-by-name, except by the magic of graph
  reduction, the evaluator can recognize when the same expression is
  going to be substituted into a function multiple times. Instead of doing the
  same evaluation steps multiple times, the evaluator remembers (or "memoizes")
  the result the first time it evaluates an expression, and just copies that
  result over if it ever has to evaluate the same expression again. The
  evaluator also doesn't bother to evaluate sub-expressions that are never actually
  used to evaluate the overall expression.


A Thunk is kind of like a function that returns (or might return) a value.

An expression like `undefined` isn't really a value, exactly. Because values
are like functions that might return a value, `undefined` is like a function
that definitely won't return a value.

```haskell
Prelude> :print undefined
undefined = (_t7::GHC.Stack.Types.HasCallStack => a7)

Prelude> undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:13:1 in interactive:Ghci12
```

We can use `:sprint` and `:print` in `GHCI` to see thunks. Let's start by
defining a value that's the result of applying some functions together:

```haskell
Prelude> let xs = map (+1) [1..10] :: [Int]
```

Printing `xs` is pretty much what you'd expect:

```haskell
Prelude> :sprint xs
xs = _
Prelude> :print xs
xs = (_t12::[Int])
```

The `_` means `xs` is a thunk, and the `_t12::[Int]`, that it's a thunk that
returns a `[Int]`:

Now let's use `seq` on `xs`, which will raise an exception if `xs` is
`undefined`:

```haskell
Prelude> seq xs ()
()
Prelude> :sprint xs
xs = _ : _
Prelude> :print xs
xs = (_t13::Int) : (_t14::[Int])
```

`GHCi` evaluated just enough of `xs` to see that wasn't `undefined`, and now
it nows that `xs` is the cons of a thunk that returns an `Int` with a thunk that
returns a `[Int]`:

```haskell
Prelude> length xs
10
Prelude> :sprint xs
xs = [_,_,_,_,_,_,_,_,_,_]
Prelude> :print xs
xs = [(_t15::Int),(_t16::Int),(_t17::Int),(_t18::Int),(_t19::Int),
      (_t20::Int),(_t21::Int),(_t22::Int),(_t23::Int),(_t24::Int)]
```

When we called `length`, `GHCi` had to walk down the graph of cons `(:)`
expressions and make sure that every tail wasn't undefined. Notice that the
items in the list could still be `undefined`, but because the list has defined
end at `[]`, the list has a defined length. Another way to write it is that `xs`
is now:

```haskell
xs = _:_:_:_:_:_:_:_:_:_:[]
```

We can then add up all the `Int`s in the list:

```haskell
Prelude> sum xs
65
Prelude> :sprint xs
xs = [2,3,4,5,6,7,8,9,10,11]
Prelude> :print xs
xs = [2,3,4,5,6,7,8,9,10,11]
```

which forces all the thunks to evaluate, and now we know what values are in
`xs`.

(above example [adapted from Parallel and Concurrent Programming in
Haskell](https://www.reddit.com/r/haskelltil/comments/2zlq40/sprint_in_ghci_lets_you_view_thunks/?st=jmdvtr71&sh=93851e55))


## 27.5 Can we make Haskell strict?


### Exercises: Evaluate

[See `Evaluate.hs`](/27/Evaluate.hs)

## 27.14 Chapter Exercises

[See `StrictList.hs`](/27/StrictList.hs)
[See `Exercise.hs`](/27/Exercise.hs)

## 27.15

1. 27.15
Follow-up resources
1. [The Incomplete Guide to Lazy Evaluation (in Haskell); Heinrich
Apfelmus](https://hackhands.com/guide-lazy-evaluation-haskell/)
2. [Chapter 2. Basic Parallelism: The Eval Monad; Parallel and
Concurrent Programming in Haskell; Simon Marlow;](http://chimera.labs.oreilly.com/books/1230000000929/ch02.html)
3. Lazy evaluation illustrated for Haskell divers; Takenobu Tani
4. A Natural Semantics for Lazy Evaluation; John Launchbury
5. An Operational Semantics for Parallel Call-by-Need; Clem Baker-Finch, David
   King, Jon Hall and Phil Trinder.

