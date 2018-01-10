--11/Hutton.hs
module Hutton where

--1
data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add a b) = (+) (eval a) (eval b) 

-- 2

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)
