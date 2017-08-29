# Exercises for Chapter 16: Functor

## Excercises: Be Kind

1. `*`
2. b is `* -> *`, T is `* -> *`
3. `* -> * -> *`

## Exercises: Heavy Lifting 
see heavylifting.hs 

## Exercises: Instances of Func
functorinstances.hs

8. Trivial doesn't have anything inside it that fmap can apply a function to,
   fmap doesn't make sense for things with kind `*`, or rather fmap on type
   constants is just function application, which is all f and no map.

## Exercise: Possibly 
possiblyeither.hs

## Short Exercise
possiblyeither.hs

2.  The `a` in `First a` might be a different type than the `b` in 
   `Second b`. The function we pass to fmap can only operate on one of those
types, but not both. In other words the function that fmap maps is of type `b
-> c` and kind `*`. Furthermore, we have to apply the function to `Second b`
rather than `First a`, because the structure that fmaps maps onto is of kind `*
-> *`. Our structure is `(Sum a)` because `Sum` is of kind `* -> * -> *` and
it needs to have accepted every type constructer but the last before its
something that fmap can work on. But that doesn't mean we're barred from
writing another function that does something different to `Sum`, but something
different wont be fmap.


## Chapter Exercises

Determine if a valid Fucntor can be written for the datatype provided:

1. No, Bool has kind `*` but fmap only works on `* -> *` 
2. Yes, note that `False'` and `True'` both take the same type `a`
3. Yes, fmap can ignore `Falsish`
4. Yes... but why...
5. Nope, theres nothing to fmap over, kind `*`

Rearrange the arguments:
rearrange.hs


Write Functor instances:
instances.hs



