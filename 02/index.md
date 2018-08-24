---
title: "Notes (HPFP 02/31): Hello Haskell"
author: jcb
date: 2017-11-01
---

# 2 Hello Haskell!

## 2.1 Hello, Haskell

**GHC**: The Glasgow Haskell Compiler is a program (written in
Haskell and C) turns Haskell code into x86 or ARM executables. There
are [many other Haskell compilers](https://wiki.haskell.org/Implementations),
but GHC is the de facto standard. Some Haskell compilers, such as Hugs, UHC
and Yhc are no longer actively developed, but are notable for historic reasons,
others, such as ghcjs and Frege (technically a separate dialect) are also
important, since they target different platforms than GHC (Javascript and the
JVM, respectively).

**GHCi**: GHC's interactive mode, or
[REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop). A
REPL, or read-eval-print-loop, reads code that you type into a command line
shell, evaluates it, prints the results, and then loops so you can type in more
code.

**Cabal**: Haskell's Common Architecture for Building Applications and
Libraries. When we write software, we often want to include external
libraries, Cabal is a tool that enables us to easily include those libraries
in our own software, as well as to publish libraries that can be used by others.

**Hackage**: A central package archive which contains most of the Haskell
libraries you're ever going to want to use. Cabal installs packages from
Hackage by default.

**Cabal hell**: When we install external libraries using cabal, those
libraries are installed globally on our machine by default. This means that
if we want to build two different projects that depend upon the same external
library, the naive way to do so is to install that library on our machine
with cabal and then include it in our projects. This seems innocuous, but
is actually very bad, because it creates a hidden interdependence between
our two projects.

  Suppose the library author added some new features in the library that we
want to use in one of our projects. It seems obvious that we could just
update the library and modify our project to use the new features, right?
Except if we do that, we might break every other project on our machine that
depends on that library, if the updated version wasn't backwards compatible.
And if the project that we're currently working on itself depends on one of
the other now-broken projects, we have to dive into a tangled web of hidden
interdependencies to get anything working. This pattern, is called
**dependency hell**, in general and is certainly not unique to cabal.  There
are a number of different ways to manage this problem, but the general
approach is to use some other tool that allows you to install multiple copies
of the same library on a per project basis so that changes in one project
don't propagate to other projects. Cabal itself provides a mechanism for
doing this which allows you to install packages into self-contained
"sandboxes," but there are other tools, such as Stack (see below), which is
what HPFP uses.

One extremely interesting way to solve the problem of dependency management
that I should note is to use a system package manager like Nix or Guix that
supports this type of package sandboxing and referentially transparent builds
for anything you install on your machine, not just Haskell packages (which is
all Cabal or Stack deals with). And you can even extend this to running an
entire OS, like NixOs or GuixSD that manages the whole OS this way. That is,
you can install the Nix package manager on e.g. macOS, but macOS packages or
applications won't be self-contained. In NixOs however, everything, including
OS libraries have isolated dependencies, which makes it radically more
difficult to break things when you, for example, update your operating system.

**Stack**: A tool for building Haskell projects and manage dependencies. I like
to think of Stack as a really nice user interface over a lot of common  Haskell
development functionality like installing packages with cabal, or managing
different versions of GHC. Stack also installs packages from Stackage which is
a mirror of Hackage that makes some guarantees about packages being compatible.
Libraries on Hackage are more like nightly or unstable builds, up to date, but
possibly brittle.  Libraries on Stackage are farther behind, but stable.

**Installing Stack**:

I started the book with [Arch Linux](archlinux.org) as my operating system,
but I switched to [NixOS](nixos.org) part of the way through. Setting up
Stack on Arch was really easy, but maintaining it proved fairly nontrivial.
Apparently, just naively updating GHC and Stack with pacman (Arch's package
manager) can result in some epic dependency hell.

When I investigated, I found a post somewhere (which I can't seem to find now)
that proposed the following:

1. Install stack with pacman
2. Install stack again with stack.
3. Use pacman to remove the system package of stack that you installed with
   pacman.
4. Use the stack that stack installed

This was too silly for me to even try, but the fact that it seemed almost
plausible told me that I should probably do my Haskell work on a different
OS. So I added a NixOS partition on my machine on the theory that it's
better to have a system that's difficult to learn and easy to debug
than the other way round.

Accordingly, installing Stack on NixOS in a way that things seem to mostly
behave was non-trivial. But it's been pretty smooth sailing ever since (knock
on wood). Here's a rough outline of what I did:

1.  Installed Stack by adding stack to my system packages in my
    configuration.nix
2.  Enabled Stack's Nix integration by adding
    ```
    nix:
      enable: true
      shell-file: shell.nix
    ```
    to the stack.yaml of any project I'm using stack with. You can also add
    ```
    nix:
      enable: true
    ```
    to your `.stack/config.yaml` if you want to globally enable stack's nix
    integration.

3.  Add the following shell.nix file to my project's root directory:
    ```
    # shell.nix

    {ghc}:
    with (import <nixpkgs> {});

    haskell.lib.buildStackProject {
      inherit ghc;
      name = "myEnv";
      buildInputs = [ zlib ];
      buildPhase = ''
        export LANG=en_US.UTF-8
      '';
    }
    ```
    The main thing this does is it allows stack to use the versions of GHC
    installed via nix from the nixpkgs repository. It also brings the `zlib`
    libray into scope and fixes an environment bug that prevents programs run by
    stack (like GHCi) from correctly displaying Unicode. There's a lot of ways
    to extend this on a per-project basis, but I've found the above to be a good
    framework.

This is a pretty boilerplatey solution, but considering this book really depends
on Stack I thought it better to stick to the text rather going the
[cabal2nix](https://github.com/NixOS/cabal2nix) route and having a more
idiosyncratic build process.

## 2.5 Understanding expressions

There's a lot of inferential distance between the previous chapter and
this one. I think it's important to point out that even though Haskell is based
on the lambda calculus, they are by no means the same thing. Lambda Calculus is
the theoretical foundation of a lot of languages, Haskell being only one of
them. But even if the foundations are the same, the structure of the languages
might be very different, based on the decisions of its designers. Start with
Lambda Calculus, build one kind of structure, and you get Haskell, but start
over and build a different kind of structure and you get Lisp.

The book skips over a lot of those design decisions and rightly so. Better
to get started playing with the language as soon as possible. But you should at
least be aware that there is some hidden complexity here. And
if you find yourself stuck on some concept, it's probably because
some implicit detail, which the author knows but you don't, is blocking you.

This happens to everyone. Just remember that every big complicated concept in
computer science is made up of little obvious steps. Computers can only do
little obvious steps, but they do very many of them very very fast. Whenever
you're stuck, go back to the last step that was obvious to you, and try
to figure out what the next obvious step is.

In this case, I highly recommend the follow-up resources in the previous
chapter, which are a great place to start at trying to close the complexity gap
between Haskell and the lambda calculus.

### Exercises: Comprehension Check

1.  `let half x = x /2`
    `let square x = x * x`

2.  `let area x = 3.14 * (square x)`
3.  `let area x = pi * (square x)`


## 2.6 Infix Operators

It may be helpful to point out that all infix operators are just syntactic
sugar over functions.

**syntactic sugar**: Syntax within a programming language that is designed to
make things easier to read or to express. It makes the language "sweeter" for
human use: things can be expressed more clearly, more concisely, or in an
alternative style that some may prefer. (from
[Wikipedia](https://en.wikipedia.org/wiki/Syntactic_sugar))

```haskell
Prelude> add = (+)
Prelude> add 2 2
4
Prelude>
```

### Exercises: Parentheses and Association
**Precedence**: Where the implicit parentheses are. Precedence of operators is
only relevant if you leave out parentheses. If you can explicitly mark
what operations you want evaluated in what order, precedence doesn't matter.
Or, in other words, parentheses have the highest precedence, just like
in grade school (the P in PEMDAS stands for parentheses).


1.  a. `8 + 7 * 9`
    b. `(8 + 7) * 9`

    a and b are different, `(*)` has a higher precedence than `(+)`

2.  a. `perimeter x y = (x * 2) + (y * 2)`
    b. `perimeter x y = x * 2 + y * 2`

    a and b are the same, `(*)` has a higher precedence than `(+)`

3.  a. `f x = x / 2 + 9`
    b. `f x = x / (2 + 9)`

    a and b are different  `(/)` has a higher precedence than `(+)`

## 2.7 Declaring values

Look at the following sequence of expressions in GHCi:

```haskell
Prelude> let x = 3
Prelude> let y = x + 1
Prelude> y
4
Prelude> let x = 7
Prelude> x
7
Prelude> y
4
```

You may have been expecting that last `y` to be `8` instead of `4`, which
is how it would work in an imperative language like C or Python.

What's curious about Haskell though is why the above sequence works
at all. Look what happens if we try to replicate
similar looking expressions in a source file:

```haskell
--- SevenIsNotThree.hs

module SevenIsNotThree where

x = 3
y = x + 1
x = 7
```

If you try loading this into GHCi:

```haskell
Prelude> :l SevenIsNotThree.hs
[1 of 1] Compiling SevenIsNotThree  ( SevenIsNotThree.hs, interpreted )

SevenIsNotThree.hs:7:1: error:
    Multiple declarations of ‘x’
    Declared at: SevenIsNotThree.hs:5:1
                 SevenIsNotThree.hs:7:1
Failed, modules loaded: none.
```

Haskell's error messages take a little getting used to, so I'll translate:

```haskell
Dear Sir or Madam,

While the Prelude module was loaded, you, the Programmer, instructed
us to load the module found in the file SevenIsNotThree.hs.

We located the aforementioned file, SevenIsNotThree.hs and attempted to load
the module SevenIsNotThree we found therein.

At line 7, column 1, in file SevenIsNotThree.hs, we encountered an error:
  You tried to declare the expression 'x' mulitple times.
  You declared 'x' at  line 5, column 1 of file SevenIsNotThree.hs
  You declared 'x' at  line 7, column 1 of file SevenIsNotThree.hs

Declaring an expression multiple times is forbidden.
  Either the declarations are the same
    and thus redundant,
  Or the declarations are different
    and thus contradictory.

We have no desire to load redundant and contradictory code. Therefore, your
request is denied and no modules were loaded.

Sincerely,
The Glorious Glasgow Haskell Compiler (interactive mode).
```

Don't worry, GHC isn't scolding you (much). You shouldn't want to
run redundant or contradictory code either. Much better to find out that your
code is broken in a message from the compiler than, for example, when your Mars
Lander crashes into the surface because your code had a type error and mixed up
feet and meters (this actually
[happened](https://en.wikipedia.org/wiki/Mars_Climate_Orbiter#Cause_of_failure)).

As for why GHC is perfectly happy to accept contradictory declarations
in GHCi let expressions but not in a source file, this is because everything
in GHCi happens inside the `IO ()` type (IO meaning input/output). `IO` let
expressions have different scope than declarations in a source file.

This seems mysterious, but will make sense in later chapters.

### Exercises: Heal the Sick

[see `HealTheSick.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/02/HealTheSick.hs)

## 2.8 Arithmetic functions in Haskell

Oh, gosh. Modular arithmetic.

My recommendation is to read the chapter and read this Wikipedia page on
the [Modulo operation](https://en.wikipedia.org/wiki/Modulo_operation).
You'll see there that pretty much every programming language has its
own idiosyncratic way of doing the modulo operation, and there isn't a lot of
consistency between them.

By the way, `divMod` and `quotRem` are two useful functions which return
pairs of `div` and `mod`, and `quot` and `rem`, respectively, applied to
the same arguments:

```haskell
divMod x y = (div x y, mod x y)
quotRem x y = (quot x y, rem x y)
```

In my opinion I wouldn't stress too much about all the detail here.
In practice, `div` and `mod` are almost always what you need, and that the
distinction between `divMod` and `quotRem` only appears when you're dealing
with negative integers. Really the difference is just what "round down" means.

Let's look at a few examples of how `divMod` and `quotRem` differ:

```haskell
Prelude> divMod 13 4
(3,1)
Prelude> quotRem 13 4
(3,1)
```

Both arguments are positive, so there is no difference. `13` divided by `4` the
ordinary way is three and a fourth, `3.25`. But since this is integer
division we round down to `3`. Since `3 * 4` is `12` the remainder is one (`13 -
12 = 1`).

But what if we do `(-13)` divided by `4`. If we were doing ordinary division
we would get negative `(-3.25)`.

Now this is the question: What does it mean to "round down" `(-3.25)`?

The two possible options are `(-3)` and `(-4)`.

If we say that `(-3.25)` rounded down is `(-3)`, then we run into the
difficulty that `(-3)` is actually greater than `(-3.25)`:

```haskell
Prelude> (-3) > (-3.25)
True
```

On the other hand, if we say that `(-3.25)` rounded down is `(-4)` then
it's the absolute value which is greater:

```haskell
Prelude> abs (-4) > abs (-3.25)
True
```

So we have a choice to make. We want integer division to "round down", but
when one of the arguments is negative, "down" can mean two different things.
We have to decide if we want "down" to be toward `0`, in which case we'll
choose `(-3)` in the above example, or towards $-\infty$, negative infinity,
in which case we choose `(-4)`.

```haskell
Prelude> divMod (-13) 4
(-4,3)
Prelude> quotRem (-13) 4
(-3,-1)
```

As we can see, `divMod` sets down as $-\infty$ and `quotRem` sets down
as `0`.

The values for `mod` and `rem`, are just consequences of that choice,
since:

```haskell
mod (-13) 4 = (-13) - 4 * (div 13 4) = (-13) - 4 * (-4) =
-13 + 16 = 3

rem (-13) 4 = (-13) - 4 * (quot 13 4) = (-13) - 4 * (-3) =
-13 + 12 = -1
```

Everything is just a consequence of what we want "down" to mean.

If we flip the signs of the arguments:

```haskell
Prelude> divMod 13 (-4)
(-4,-3)
Prelude> quotRem 13 (-4)
(-3,1)
```

`div` and `quot` are the same, because `13` divided by `(-4)` is still `(-3.25)`
and the decision about "down" is the same.

`mod` and `rem` are different though, because:

```haskell
mod 13 (-4) = 13 - (-4) * (div 13 4) = 13 + 4 * (-4) =
13 - 16 = -3

rem 13 (-4) = 13 - (-4) * (quot 13 4) = 13 + 4 * (-3) =
13 - 12 = 1
```

Lastly, if we set the signs of both arguments negative:

```haskell
`(-13)` / `(-4)` = 13 / 4 = 3.25
```

and we're back to "down" being unambiguous, so `divMod` and `quotRem`
are the same again.

```haskell
Prelude> divMod (-13) (-4)
(3,-1)
Prelude> quotRem (-13) (-4)
(3,-1)
```

But really. Just use `divMod`. You almost always want "down" to be towards
negative infinity.

I would almost rather manually recreate `quot` out of `div` wherever I
needed it:

```haskell
quot x y = (sign x) * (sign y) * (div (abs x) (abs y)) where
  sign x = div (abs x) x
```

Just so that it's perfectly clear that I'm really interested in rounding
absolute values.


## 2.9 Parenthesization

`$`, or "apply", is a great help in making Haskell code legible.

Look at the difference between

```haskell
fun x = (foo (bar (baz x)))

fun' x = foo $ bar $ baz x
```

I think the latter is a lot nicer. But the difference gets even starker
when you have a more complicated syntax structure:

```haskell
lessFun x = (qux (baz (bar x)) (foo (bar x)) (baz (quux (foo (bar x)) (foo x))))
```

My eyes glaze over after the first bar.

```haskell
moreFun x = qux (baz $ bar x) (foo $ bar x) (baz $ quux (foo $ bar x) (foo x))
```

Now it's a lot easier to tell that qux takes three arguments.

We could also do:

```haskell
moreFun x = qux (baz $ bar x) (foo $ bar x) $ baz $ quux (foo $ bar x) $ foo x
```

But I think this is less clear. In general I like to use parentheses and `$`
to better illuminate the structure of whatever the expression is, which
depending, on the specific expression that might require slightly different
styles. `let` and `where` expressions can also be a great help.

```haskell
moreFun' x = qux (baz $ bar x) fbx (baz $ quux fbx $ foo x) where
  fbx = foo $ bar x
```


### Exercises: A Head Code

1. `let x = 5 in x`

    returns 5

2. `let x = 5 in x * x`

    returns 25

3. `let x = 5; y = 6 in x * y`

    returns 30

4. `let x = 3; y = 1000 in x + 3`

    returns 6

**Rewrite with where clauses**:

1. `x * 3 + y where x = 3; y = 1000`
2. `x * 5 where y = 10; x = 10 * 5 + y`
3. `z / x + y where x = 7; y = negate x; z = y * 10`

## 2.11 Chapter Exercises

### Parenthesization

1. `2 + (2 * 3) - 1`
2. `(^) 10 $ (1 + 1)`
3. `(2^2) * (4^5) + 1`

### Equivalent expressions

1. `1 + 1` returns the same as `2`

2. `10^2` returns the same as `10+9*10`

3. `400 - 37` is not the same as `(-) 37 400`, different argument order.

4. ``100 `div` 3`` is not the same as `100 / 3`, former is integer division vs
fractional division for the latter.

5. `2 * 5 + 18` is not the same as `2 * (5 + 18)`, operator precedence

### More fun with functions

1.  `10 + waxOn` will return 1135

    `(+10) waxOn` will return 1135

    `(-) 15 waxOn` will return -1110

    `(-) waxOn 15` will return 1110

2. Nothing to do for this exercise

3. 3375

4.  [see `MoreFun.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/02/MoreFun.hs)
5.  [see `MoreFun.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/02/MoreFun.hs)
6.  [see `MoreFun.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/02/MoreFun.hs)

7.  `waxOff 10` returns `30`

    `waxOff (-50)` returns `(-150)`

## 2.13 Follow-up resources

1. [Haskell wiki article on Let vs Where](https://wiki.haskell.org/Let_vs._Where)

2. [How to desugar Haskell code; Gabriel Gonzalez](https://github.com/johnchandlerburnham/hpffp-resources/blob/master/Chapter-02/How%20to%20desugar%20Haskell%20code.pdf)

---
