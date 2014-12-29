data Expr = Add Expr Expr | Val Int

eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y
 