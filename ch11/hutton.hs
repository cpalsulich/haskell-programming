data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit val) = val
eval (Add expr1 expr2) = (eval expr1) + (eval expr2)

printExpr :: Expr -> String
printExpr (Lit val) = show val
printExpr (Add expr1 expr2) = concat [(printExpr expr1), " + ", (printExpr expr2)]
