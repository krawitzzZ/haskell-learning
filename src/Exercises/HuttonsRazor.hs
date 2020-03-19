module Exercises.HuttonsRazor where

data Expr = Lit Integer | Add Expr Expr deriving (Show, Eq, Ord)

eval :: Expr -> Integer
eval (Lit x   ) = x
eval (Add x x') = eval x + eval x'

printExpr :: Expr -> String
printExpr (Lit x   ) = show x
printExpr (Add x x') = printExpr x ++ " + " ++ printExpr x'
