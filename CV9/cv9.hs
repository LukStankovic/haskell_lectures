data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Char
      deriving (Eq)
      
test1 :: Expr
test1 = Add (Num 1) (Num 2)

test2 :: Expr
test2 = Add( Mul (Add (Num 1) (Num 2)) (Add (Num 4) (Var 'x'))) (Mul (Num 1) (Var 'x'))

-- (Add (Num 4) (Num 3)

eval :: Expr -> Int
eval (Num x) = x
eval (Add l r) = (eval l) + (eval r) 
eval (Sub l r) = (eval l) - (eval r)
eval (Mul l r) = (eval l) * (eval r)
eval (Div l r) = (eval l) `div` (eval r)


showExpr :: Expr -> String
showExpr expr = showExpr' False expr where
    add x = "(" ++ x ++ ")"

    showExpr' :: Bool -> Expr -> String
    showExpr' _ (Num x) = show x
    showExpr' _ (Var x) = [x]
    showExpr' high (Add l r) = 
        let result = (showExpr' False l) ++ "+" ++ (showExpr' False r) 
        in if high then add result else result    
    showExpr' high (Sub l r) = 
        let result = (showExpr' False l) ++ "+" ++ (showExpr' False r) 
        in if high then add result else result    
    showExpr' _ (Mul l r) = (showExpr' True l) ++ "*" ++ (showExpr' True r)
    showExpr' _ (Div l r) = (showExpr' True l) ++ "/" ++ (showExpr' True r) 


instance Show Expr where 
    show = showExpr

deriv :: Char -> Expr ->Expr
deriv _ (Num _) = (Num 0)
deriv x (Add l r) = Add (deriv x l) (deriv x r)
deriv x (Sub l r) = Sub (deriv x l) (deriv x r) 
deriv x (Mul l r) = Add (Mul (deriv x l) r) (Mul l (deriv x r))
deriv x (Div l r) = Div (Sub (Mul (deriv x l) r) (Mul (deriv x r) l)) (Mul r r)
deriv x (Var y) |  x == y = (Num 1)
                | otherwise = (Num 0)