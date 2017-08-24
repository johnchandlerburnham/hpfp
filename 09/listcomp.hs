module ListComp where

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

ex1 = [(x, y) | x <- mySqr, y <- myCube]
ex2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
ex3 = length ex2
