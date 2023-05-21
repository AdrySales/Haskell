
--QUESTAO NUMERO 1
replace :: String -> String -> String -> String
replace _ _ [] = []
replace (s1:xs1) (s2:xs2) (s3:xs3)
                 | take len (s3:xs3) == (s1:xs1) = (s2:xs2) ++ replace (s2:xs1) (s2:xs2) (drop len (s3:xs3))
                 | otherwise = s3 : replace (s1:xs1) (s2:xs2) xs3
                 where len = length (s1:xs1)



--QUESTAO NUMERO 2
data Expr = Tr
           |Fa                
           |AND Expr Expr
           |OR Expr Expr
           |NOT Expr


eval :: Expr -> Bool
eval Tr = True
eval Fa = False
eval AND e1 e2 = eval e1 && eval e2
eval OR e1 e2 = eval e1 || eval e2
eval NOT e1 = not (eval e1)