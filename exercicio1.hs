type Chave = [(Char, Char)]
rot13parcial :: Chave -- troca 'a' por 'n', 'b' por 'o' etc.
rot13parcial = [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),('f','s'),('g','t'),('h','u'),('i','v'),('j','w'),('k','x'),('l','y'), ('m','z')]

suzuka :: Chave -> Char -> Char
suzuka [] b = b
suzuka ((a,s):as) b | a == b = s
                    | otherwise = (suzuka as b)

cipher :: Chave -> String -> String
cipher _ [] = []
cipher cha (a:as) =  (suzuka cha a):[]  ++ (cipher cha as)
 

 --deixar uma palavra na cifra rot13, chamada: cipher rot13parcial "hello"


--module Main (main) where
--main :: IO ()
--main = do putStrLn "Qual é o seu nome? "
--                   nome <- getLine
--                   putStr nome
--                  putStrLn ", seja bem vindo(a)!"

-- vabs :: Integer -> Integer
--vabs n | n > 0 = 1
--             | n == 0 = 0
--             |otherwise  = -1

--limpar o buffer em cada entrada de dados : hFlush stdout

-- tail -pega a cabeça | lenght - tamanho da lista | 