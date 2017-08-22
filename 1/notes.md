# Notes for Chapter 1

## 1.1: All You Need is Lambda

- Lambda Calculus formalizes effective computability.
- Don't skip lambda calculus. 

What is Computability? What are Turing Machines? 

## 1.2 What is functional programming

- Expressions are values, variables or functions.  
- Functions are expressions applied to arguments and reduced or evaluated.

Functions are first-class, can be values or arguments to other functions.

Haskelll is pure, which means referentially transparent, which means for any
given function, if you pass it the same arguments it evaluates to the same
eturn value. Same inputs, same outputs.

## 1.3 What is a function

A function is a relation between a set of inputs and a set of outputs

This isn't a bad section but really, for a beginner, more work with functions
might be helpful. I might do a Functions Made Easy tutorial just for this
reason.

Also, the idea of functions being relations between sets means that you need
to have some idea of what a set is, mathematically speaking. Most people
would not, in colloquial language, call the empty set a set. 

Bob: Hey Alice! Nice set of golf clubs!
Alice: Bob, what are you talking about? I don't have any golf clubs.
Bob: Exactly! Nice empty set of golf clubs!

But if a function is relation between sets, then there are functions which 
can output the empty set, especially when division by zero gets involved.

## 1.4 The Structure of lambda terms

Lambda calculus has three components: expression, variable, abstraction

An expression is a variable, an abstraction or a combination thereof

[this seems imprecise in the book... should find better lambda calculus intro]

Abstraction is a function with a head (lambda and variable name) and a body (an
expression). Head variable is the parameter.

Lambda abstractions are anonymous functions.

See, this is why more work on functions was needed. They just introduced idea
of a parameter and the idea that functions can have names or not have names.
You could have just done this in normal function-land instead of
lambda-expression-land. 

That might be a nitpick though, this is still one of the more thorough
introductions to a topic from first principles I've seen. Okay it's not
nand2tetris, but nothing is nand2tetris.

Alpha equivalence: You can change the name of any variable, as long as you
change all instances of that name consistently and you don't change it to a
name that already exists in the function elsewhere. 

This would be a lot easier to get with an explanation of what scope was in the
section that should come before this on functions.

## 1.5 Beta Reduction

  Same comment here. In function-land, we can think about the idea of giving a
function an input. So if we let f(x) = x, what operation takes the function
f(x) and the number 4 and gives you whatever f(4) is? In other words, what
applies f(x) to 4?

Free variables: Okay, seriously, there's no excuse for not using all these
terms in functionland first. I'm repeating myself. I need to stop.
## 1.6 Multiple Arguments

## 1.7 Evaluation is simplification






