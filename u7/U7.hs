data Expr = Addi Expr Expr | Mult Expr Expr | Sub Expr Expr | Divi Expr Expr | Numb Float deriving (Show,Eq)

eval::Expr->Float
eval (Numb a) = a
eval (Addi a b) = eval a + eval b
eval (Mult a b) = eval a * eval b
eval (Sub a b) = eval a - eval b
eval (Divi a b) = eval a / eval b

--AUFGABE 4
data Expr1 = App Expr1 Expr1 | S | K | I | Var String | Lam String Expr1 deriving (Show,Eq)

transform (Lam x y) = (eliminate x y)
eliminate x S = App K S
eliminate x K = App K K
eliminate x I = App K I
eliminate x (Var y)
                    | x==y      = I
                    | otherwise = (App K (Var y))
eliminate x (Lam y z) = eliminate x (eliminate y z)
eliminate x (App y z) = (App (App S (eliminate x y)) (eliminate x z))

zzero = transform (Lam "s" (Lam "z" (Var "z")))
succesor = transform (Lam "w" (Lam "x" (Lam "y" (App (Var "x") (App (App (Var "w") (Var "x")) (Var "y"))) )))