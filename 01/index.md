---
title: "Notes (HPFP 01/31): All You Need is Lambda"
author: jcb
date: 2017-11-01
---

# 1 All You Need is Lambda

## 1.3 What is a Function?
**Function**: A relation between an input set and output set such that each
input has only one ouput. Personally, I think its more intuitive to imagine a
function as a transformation that changes one type of thing (the input set)
into another type of thing (the output set). I like to imagine a vegetable
juicer: Put carrots in, get carrot juice out. Put spinach in, get spinach juice
out. What is the *function* of a vegetable juicer? It transforms vegetables
into vegetable juice. The set of things that can go into a vegetable juicer are
its input set (or *domain*) and the set of things that can come out are its
output set (or *codomain*).

**Computable functions**: If a function is a transformation between two sets of
things, and we can build machines which do transformations, then any function
for which we can build a machine is called a **computable** function. Not all
functions are computable.

**Turing Machines**: A simple model of a computer based on manipulating symbols
on a tape according to some rules and an internal state. Despite its
simplicity, a Turing Machine can be built that executes any computable
function.

**Lambda Calculus**: A simple model of computation based on building,
applying, and evaluating functions. A function built using the Lambda
Calculus is called a lambda expression, and for any computable function,
a lambda calculus can be constructed which evaluates that function.

**parameter**: An input to a function.

**"functions are first-class"**: Functions can be parameters.

**referential transparency**: The property that the same function with
the same parameters returns the same output. All functions are referentially
transparent over all their parameters, but in some situations it can
be difficult to tell what all a functions' parameters are. Suppose you turn
a dozen carrots into carrot juice with a vegetable juicer, don't clean the
juicer afterwards. You'll drink your carrot juice, but the next person to use
the juicer will have carrot flavored juice, even if they wanted celery and
ginger. The juicer function has hidden paramters, i.e.  the residue of previous
inputs. Functions like this can be described as *stateful*, because they have
some internal or hidden parameters whose state can affect the output of the
whole function.

**purity**: A synonym for referential transparency.

**Functional programming**: Building machines (programs) that evaluate
computable functions using the lambda calculus.

## 1.4 The Structure of lambda terms

**abstraction**: A lamda expression that represents a function. It can be
written down with the folowing notation: $`\lambda x.x`$. Everything between the
$`\lambda`$ and the $`.`$ is called the **head**, and the symbol in the head names
the parameter of the function. After the $`.`$ is the **body**, and describes
what to do with the parameter, when the abstraction is applied. Terms that
occur in both the **head** and the **body** are called *bound variables*, and
symbols that only appear in the body are called *unbound*, or *free variables*.

**currying**: Properly speaking, all abstractions have only single parameters.
Functions with multiple paramters are expressed as a nested strucure of single-
parameter functions. For example $`\lambda xy.xy`$ is more properly written as
$`\lambda x.(\lambda y.xy)`$.

**application**: A lambda expression can be applied to some input like so:
$`((\lambda x.x) N)`$. Following the reductions steps will evaluate the function
described by $`\lambda x.x`$ with input parameter N.


## 1.5 Beta reduction

**alpha equivalence**: Within an abstraction, the specific symbols in the head
may be replaced by other symbols as long as the replacement is consistent and
total. For example, in the abstraction $`\lambda x.x`$ the term $`x`$ in the head
may be replaced with $`y`$, so long as all instance of $`x`$ in the body are also
replaced with $`y`$. Thus, $`\lambda x.x`$, $`\lambda y.y`$, and any other
expression of the form $`\lambda n.n`$ (for some $`n`$) are alpha equivalent.

**beta reduction**: An abstraction is evaluated by replacing all its bound
variables with the expression the abstraction is evaluated against (its input),
and then removing the head of the abstraction. For example, $`((\lambda x.x) N)`$
would be evaluated by replacing all $`x`$'s in the body with $`N`$ (yielding
$`\lambda x.N`$) and then removing the head for the final output of $`N`$.

### Intermission: Equivalence Exercises (p.13)

1. `\xy.xz` is equivalent to `\mn.mz`, by alpha equivalence of `x` with `m`
and `y` with `n`.
2. `\xy.xxy` is equivalent to `\a.(\b.aab)`, by currying and alpha equivalence.
3. `\xyz.zx` is equivalent to `\tos.st`.

## 1.7 Evaluation is simplification

**beta normal form**: When an expression cannot be further reduced through
*beta reduction* (i.e. application of abstractions). This signals the end
of the evaluation.

**combinator**: A lambda term with no free variables. $`\lambda x.x`$ is a combinator,
$`\lambda x.xy`$ is not.

**divergence**: If an expression can never reach *beta normal form*, it
is said to diverge. For example, $`(\lambda x.xx)(\lambda x.xx)`$ diverges.
This corresponds to non-terminating function (an infinite loop).

## 1.11 Chapter Exercises (p.17)

### Combinators
1. `\x.xxx` is a combinator.
2. `\xy.zx` is not a combinator, `z` is free.
3. `\xyz.xy(zx)` is a combinator.
4. `\xyz.xy(zxy)` is a combinator
5. `\xy.xy(zxy)` is not a combinator, `z` is free

### Normal form or diverge?
1. `\x.xxx` is Normal
2. `(\z.zz)(\y.yy)` diverges
3. `(\x.xxx)z` is Normal

### Beta Reduce

1.  `(\abc.cba)zz(\wv.w)`

    Reduction:
    ```
    (\abc.cba)zz(\wv.w) ->
    (\bc.cbz)z(\wv.w) ->
    (\c.czz)(\wv.w) ->
    (\wv.w)zz ->
    (\v.z)z ->
    z
    ```

2.  `(\x.\y.xyy)(\a.a)b`

    Reduction:
    ```
    (\x.\y.xyy)(\a.a)b ->
    (\y.(\a.a)yy)b ->
    (\a.a)bb ->
    bb
    ```


3.  `(\y.y)(\x.xx)(\z.zq)`

    Reduction:
    ```
    (\y.y)(\x.xx)(\z.zq) ->
    (\x.xx)(\z.zq) ->
    (\z.zq)(\z.zq) ->
    (\z.zq)q ->
    (\z.zq)q ->
    qq
    ```

4.  `(\z.z)(\z.zz)(\z.zy)`

    Reduction:
    ```
    (\z.z)(\z.zz)(\z.zy) ->
    (\z.zz)(\z.zy) ->
    (\z.zy)(\z.zy) ->
    (\z.zy)y ->
    yy
    ```

5.  `(\x.\y.xyy)(\y.y)y`

    Reduction:
    ```
    (\x.\y.xyy)(\y.y)y ->
    (\y.(\y.y)yy)y
    (\y.y)yy
    yy
    ```

6.  `(\a.aa)(\b.ba)c`

    Reduction:
    ```
    (\a.aa)(\b.ba)c ->
    (\b.ba)(\b.ba)c ->
    (\b.ba)ac ->
    aac
    ```

7.  `(\xyz.xz(yz))(\x.z)(\x.a)`

    Reduction:
    ```
    (\xyz.xz(yz))(\x.z)(\x.a) ->
    (\z.(\x.z1)z((\x.a)z))
    (\z.z1a)
    ```

## 1.14 Follow-up Resources

1.  [Raul Rojas. A Tutorial Introduction to the Lambda
    Calculus](http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf)

    [Notes: A Tutorial Introduction to the Lambda Calculus
    (Rojas)](/notes/tilc/00.html)

2.  [Henk Barendregt; Erik Barendsen. Introduction to Lambda
    Calculus](http://www.cse.chalmers.se/research/group/logic/TypesSS05/
    Extra/geuvers.pdf)

    [Notes: Introduction to Lambda Calculus by Henk Barendregt & Erik
    Barendsen](/notes/lcbb/00.html)

3.  [Jean-Yves Girard; P. Taylor; Yves Lafon.
    Proofs and Types](http://www.paultaylor.eu/stable/prot.pdf)

    [Notes: Proofs And Types by Jean-Yves Girard
    Barendsen](/notes/prot/00.html)

---
