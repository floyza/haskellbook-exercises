module AlgebraicDatatypes.HuttonRazor where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Add lhs rhs) = eval lhs + eval rhs
eval (Lit i) = i

printExpr :: Expr -> String
printExpr (Add lhs rhs) = printExpr lhs ++ " + " ++ printExpr rhs
printExpr (Lit i) = show i
