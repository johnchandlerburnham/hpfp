# Exercises for Chapter 14: Testing

## Intermission: Short Exercise
 see addition/Addition.hs

## Chapter Exercises

### Validating numbers into words
see exercises/test/Spec.hs

### Using QuickCheck
see exercises/test/Spec.hs

### Failure
Irrational numbers like e.g. the sqaure root of 2 cannot be represented
with infinite precision in a finite amount of memory. So an expression
like (sqrt 2) is not actually equal to the square root of 2, but rather
is an accurate approximation to some precision. So even though square
is the inverse of square root, because sqrt cannot be infinitely accurate
the square of a square root will have some error. E.g.

sqrt 2 = 1.4142135, (sqrt 2) ^ 2 = 1.9999999

### Idempotence
see exercises/test/Spec.hs

### Make a Gen random generator for the datatype
see exercises/test/Spec.hs

### Hangman testing 
### Validating ciphers
skipping these, I think that this testing chapter is probably better 
understood after you understand what monads are. The reader here only barely
has the tools to build something for which testing is important.




