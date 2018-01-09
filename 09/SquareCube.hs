--09/SquareCube.hs
module SquareCube where

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

exercise1 = [(x, y) | x <- mySqr, y <- myCube]
exercise2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
exercise3 = length exercise2
