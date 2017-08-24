# Exercises for Chapter 7: More functional patterns


## Exercises: Grab Bag

1. They are all equivalent
2. d
3. 
- a. `f = \x -> x + 1`
- b. `addFive = \x -> \y -> (min x y) + 5`
- c. `mflip f x y = f y x`

## Exercises: Variety Pack

1.
a. `k :: (a, b) -> a`
b. `k2` is a `String`, not the same as `k1` or `k3`
c. `k1` and `k3`

2. `f (a, b, c) (d, e, f) = ((a, d), (c, f))`


## Exercises: Case Practice

1. 
```
functionC x y =
  case (x > y) of
    True -> x 
    False -> y 
```

2. 
```
ifEvenAdd2 n = 
  case even n
    True -> (n + 2)
    False -> n
```

3. 
```
nums x = 
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
```


## Exercises: Artful Dodgy 

2. `11`
3. `22`
4. `21`
5. `12`
6. `11`
7. `21`
8. `21`
9. `22`
10. `31`
11. `23`

## Exercises: Guard Duty

see avgGrade.hs

1. Can't do otherwise if theres no wise to other. 
2. No, because the conditions are not exclusive.
3. b
4. anything reversible, so lists
5. `[a] -> Bool`
6. c
7. `(Ord a, Num a) => a`
8. `(Ord a, Num a) => a -> Bool`


## Chapter Exercise

### Multiple Choice

1. d
2. b
3. a
4. b
5. a

### Let's write code

1. 
- a. see cex1.hs, using `divMod` seems superfluous, `div` looks like it
  works perfectly well, unless theres some detail I've overlooked.
- b. Yes
- c. `hunsD x = div x 100`.

2. see cex2.hs
3. `g f (a, b) = (f a, b)`
4. see arith4.hs 
5. see arith4.hs
6. see arith4.hs


