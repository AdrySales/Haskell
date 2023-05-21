main :: IO ()
main = imprimeTabela 4

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(cabecalho 
                   ++ imprimeSemanas n
                   ++ imprimeTotal n
                   ++ imprimeMedia n
                  )

cabecalho :: String
cabecalho = "Semana   Vendas\n"

imprimeTotal :: Int -> String
imprimeTotal n = "Total" ++ paraDireita 6 (show (totalVendas n)) ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = "Média" ++ paraDireita 7 (show (mediaVendas n)) ++ "\n"

imprimeSemanas :: Int -> String
imprimeSemanas 0 = paraDireita 3 "0" ++ paraDireita 7 (show (vendas 0)) ++ "\n"
imprimeSemanas n = imprimeSemanas (n-1) ++
                   paraDireita 3 (show n) ++ paraDireita 7 (show (vendas n)) ++ "\n"


mediaVendas :: Int -> Float
mediaVendas n = fromIntegral (totalVendas n) / fromIntegral (n+1)

vendas :: Int -> Int
vendas 0 = 50
vendas 1 = 70
vendas 2 = 90
vendas 3 = 120
vendas 4 = 100
vendas n = 0

totalVendas :: Int -> Int
totalVendas n | n == 0    = vendas 0
              | otherwise = totalVendas (n-1) + vendas n

maxVendas :: Int -> Int
maxVendas n  | n == 0    = vendas 0
             | otherwise = max (maxVendas (n-1)) (vendas n)


addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n-1)
--addEspacos n | n == 0 = ""
--             | otherwise = " " ++ addEspacos (n-1)

paraDireita :: Int -> String -> String
paraDireita n str = addEspacos n ++ str


