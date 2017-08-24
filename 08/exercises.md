# Exercises for Chapter 8: Recursion

## Intermission: Exercises

```
applyTimes 5 (+1) 5 ->
(((+1) . (+1) . (+1) . (+1) . (+1)) 5)
```

## Chapter Exercises

### Review of types

1. d
2. b
3. d
4. b

### Reviewing currying

```
flippy :: String -> String -> String
appedCatty :: String -> String
frappe :: String -> String
```

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

2. cex2.hs
3. cex3.hs

### Fixing dividedBy:
see dividedBy.hs

### McCarthy 91 function:
see mccarthy91.hs

### Numbers into Words: 
see wordnumber.hs

