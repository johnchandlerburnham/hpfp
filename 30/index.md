# 30 When things go wrong

## 30.9 Asynchronous Exceptions

Looks like I can't get `test.dat` to have all `0`'s. If the thread exceptions
the file ends up empty. Maybe GHC changed the way it deals with writing files
since this was written?

## 30.10 Follow-up Reading

1. [A Beginnner's Guide to Exceptions in Haskell; Erin
   Swenson-Healey](https://www.youtube.com/watch?v=PWS0Whf6-wc)
2. [Chapter 8. Overlapping Input/Output; Parallel and Concurrent Programming in
   Haskell; Simon Marlow;](http://chimera.labs.oreilly.com/books/1230000000929/ch08.html)
3. [Chapter 9. Cancellation and Timeouts; Parallel and Concurrent Programming in
   Haskell; Simon Marlow;]( http://chimera.labs.oreilly.com/books/1230000000929/ch09.html)
4. [An Extensible Dynamically-Typed Hierarchy of Exceptions; Simon
   Marlow](http://community.haskell.org/~simonmar/papers/ext-exceptions.pdf)
