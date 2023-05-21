--faca um programa em haskell que dado um numero n e uma lista, devolva uma lista com os numeros multiplos de 3 e maiores que n

funcao1 :: Int -> [Int] -> [Int]
funcao1 _ [] = []
funcao1 n (x:xs) 
               | ((x>n) && ((mod x 3)==0) ) = x:(funcao1 n xs)
               | otherwise = funcao1 n xs


--dado um numero N e uma lista, faca uma funcao que devolva uma lista contendo os n primeiros numeros da lista dada

funcao2 ::  Int -> [t] -> [t]
funcao2 _ [] = []
funcao2 n lista = take n lista

--faca uma funcao que recebe 2 listas e retorna a intersecao entre elas
funcao3 :: [t] -> [t] -> [t]
funcao3 lista1 lista2 = lista1 ++ lista2

--verifica se numero e primo
e_primo :: Int -> Bool
e_primo n= if((length [x | x<-[1..n], mod n x ==0 ])>2) then False else True

--fatorial
fatorial :: Int -> Int
fatorial 0 =0
fatorial 1=1
fatorial n= n* (fatorial (n-1))

--fibonachi
fib :: Int -> Int
fib 1 = 0
fib 2= 1
fib 3=1
fib n = (fib (n-1)) + (fib (n-2))

--produto
produto :: Int -> Int
produto n = product [1..n]

--verificar o maior numero em uma lista
verifica :: [Int] -> Int
verifica [x] = x
verifica (x:xs) | (x > (verifica xs)) = x
                | otherwise = verifica xs

--verifica se um numero pertence a uma lista
pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence n (x:xs) | (n==x) = True 
                  | otherwise = (pertence n xs)

--retornar todos os pares 
pares :: [Int] -> [Int]
pares []= []
pares (x:xs) | ((mod x 2)==0) = x : (pares xs)
             | otherwise= pares xs