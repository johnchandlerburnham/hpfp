# Exercises for Chapter 3: Strings

## Exercises: Scope

1. `y` is in scope for `z`
2. `h` is not in scope for `g`
3. `pi` is in scope, since it's in Prelude, but `d` is not in scope for 
the definition of `r`. `d` is an argument in `area` but `r` can't see into
`area`'s definition.
4. This fixes what was wrong in the previous question. The `where` clause means
that `r` can access `area`'s arguments. However, `r` is now locally defined to
`area`, so another top level function wouldn't be able to see it.


## Exercises: Syntax Errors

1. Need (++) not ++
2. Single quotes are type Char not type String
3. This one works.

## Chapter Exercises

### Reading syntax:

1. a. correct
   b. not correct, need `(++)` not `++`
   c. correct
   d. incorrect, string not closed
   e. incorrect, wrong arg order
   f. correct
   g. incorrect, no integer argument
   h. correct

2. a. d
   b. c
   c. e
   d. a
   e. b

### Building Functions:

1. a. `(++ "!")`
   b. `(!! 4)`
   c. `(drop 6)`

2. see ex2.hs
3. see ex3.hs
4. see ex4.hs
5. see ex5.hs
6. see ex5.hs
 
