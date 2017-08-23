# Exercises for Chapter 2: Basic Expressions and Functions

## Comprehension Check

1. let half x = x /2
   let square x = x * x

2. let area x = 3.14 * (square x)
3. let area x = pi * (square x)

## Parentheses and Association

1. a. `8 + 7 * 9`
   b. `(8 + 7) * 9`
   a and b are different, `(*)` has a higher precedence than `(+)`

2. a. `perimeter x y = (x * 2) + (y * 2)`
   b. `perimeter x y = x * 2 + y * 2`
   a and b are the same, `(*)` has a higher precedence than `(+)`

3. a. `f x = x / 2 + 9`
   b  `f x = x / (2 + 9)`
   a and b are different  `(/)` has a higher precedence than `(+)`

## Heal the Sick

1. `let area x = 3.14 * (x * x)`
2. `let double x = x * 2`

3.  source file:
    ```
    x = 7
    y = 10
    f = x + y
    ```
 
## A Head Code 

1. `let x = 5 in x` returns 5
2. `let x = 5 in x * x` returns 25
3. `let x = 5; y = 6 in x * y` returns 30
4. `let x = 3; y = 1000 in x + 3` returns 6

Rewrite with where clauses:

1. `x * 3 + y where x = 3; y = 1000`
2. `x * 5 where y = 10; x = 10 * 5 + y`
3. `z / x + y where x = 7; y = negate x; z = y * 10`

## Chapter Exercises

### Parenthesizations

1. `2 + (2 * 3) - 1`
2. `(^) 10 $ (1 + 1)`
3. `(2^2) * (4^5) + 1`

### Equivalent expressions

1. `1 + 1` returns the same as `2`
2. `10^2` returns the same as `10+9*10`
3. `400 - 37` is not the same as `(-) 37 400`, different argument order.
4. ``100 `div` 3`` is not the same as `100 / 3`, former is integer division vs
floating point division for the latter.
5. `2 * 5 + 18` is not the same as `2 * (5 + 18)`, operator precedence

### More fun with functions

1. `10 + waxOn` will return 1135
   `(+10) waxOn` will return 1135
   `(-) 15 waxOn` will return -1110
   `(-) waxOn 15` will return 1110

2. Nothing to do for this exercise
3. 3375

4. see morefun.hs
5. see morefun.hs
6. see morefun.hs
7. `waxOff 10` returns `30`
   `waxOff (-50)` returns `(-150)`

