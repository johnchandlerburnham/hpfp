---
title: "Notes (HPFP 09/31): Lists"
---

# 9 Lists

## 9.5 Using ranges to construct lists

### Exercise: EnumFromTo

[see `EnumFromTo.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/09/EnumFromTo.hs)

## 9.6 Extracting portions of lists

### Exercises: Thy Fearful Symmetry

[see `FearfulSymmetry.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/09/FearfulSymmetry.hs)

## 9.7 List Comprehensions

### Exercises: Comprehend Thy Lists

1. `[4,15,36,64,100]`
2.
    ```haskell
    [(1,1),(1,4),(1,9),(1,16),(1,25),(1,36),(1,49)
    ,(4,1),(4,4),(4,9),(4,16),(4,25),(4,36),(4,49)
    ,(9,1),(9,4),(9,9),(9,16),(9,25),(9,36),(9,49)
    ,(16,1),(16,4),(16,9),(16,16),(16,25),(16,36),(16,49)
    ,(25,1),(25,4),(25,9),(25,16),(25,25),(25,36),(25,49)
    ,(36,1),(36,4),(36,9),(36,16),(36,25),(36,36),(36,49)
    ,(49,1),(49,4),(49,9),(49,16),(49,25),(49,36),(49,49)
    ]
    ```

3. `[(1,1),(1,4),(1,9),(1,16),(1,25)]`

### Exercises: Square Cube

[see `SquareCube.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/09/SquareCube.hs)

## 9.8 Spines and nonstrict evaluation

### Exercises: Bottom Madness

1. bottom
2. works
3. bottom
4. works
5. bottom
6. works
7. bottom
8. works
9.  works
10. bottom

### Intermission: Is it in normal form?

1. WHNF & NF
2. WHNF
3. neither
4. neither
5. neither
6. neither
7. WHNF

## 9.9 Transforming lists

### Exercises: More Bottoms

1. bottom
2. yes
3. bottom
4. Is this character a vowel?
  `itIsMystery :: Char -> Bool`
5.  a. the first 10 square numbers
    b. `[1, 10, 20]`
    c. `[15, 15, 15]`

6. [see `FoldBool.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/09/FoldBool.hs)

## 9.10 Filtering lists of values

### Exercises: Filtering

[see `Filtering.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/09/Filtering.hs)

### Zipping exercises

[see `ZippingExercises.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/09/ZippingExercises.hs)

## 9.12 Chapter Exercises

### Data.Char

[see `CharExercises.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/09/CharExercises.hs)

### Ciphers

[see `Cipher.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/09/Cipher.hs)

### Writing your own standard functions

[see `StdFunc.hs`](https://github.com/johnchandlerburnham/hpfp/blob/master/09/StdFunc.hs)

## 9.14 Follow-up resources

1. [Data.List documentation for the base library.](http://hackage.haskell.org/package/base/docs/Data-List.html)

2. [Ninety-nine Haskell problems.](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)

---
