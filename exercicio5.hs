data KeyTree = Node Char Char KeyTree KeyTree
             | Empty
            deriving (Show)
 
chaveParcial :: KeyTree
chaveParcial = Node 'h' 'u' (Node 'c' 'p' (Node 'b' 'o' (Node 'a' 'n' Empty Empty) Empty) (Node 'e' 'r' Empty Empty)) (Node 'l' 'y' Empty (Node 'm' 'z' Empty Empty))
 
cipherT :: KeyTree -> String -> String
cipherT _ [] = []
cipherT Empty (x:xs) = x : cipherT chaveParcial xs
cipherT (Node c1 c2 tEsq tDir) (x:xs) | x == c1 = c2 : cipherT chaveParcial xs
                                      | x < c1 = cipherT tEsq (x:xs)
                                      | otherwise = cipherT tDir (x:xs)
                               