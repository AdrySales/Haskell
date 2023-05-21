--Aluna: Yasmim Adrieny da Silva Sales - 10131057456

--Primeira Questao 
merge :: [Int] -> [Int] -> [Int]
merge [] l1=l1
merge l2 []=l2
merge (l1:lista1)(l2:lista2)  | l1 < l2 = l1: (merge lista1 (l2:lista2)) -- menor
                              | l1 > l2 = l2: (merge lista2 (l1: lista1)) -- maior 
                              | l1 ==l2 = l1: l2:(merge lista1 lista2)    --igual










  toNumber :: Char -> Int
toNumber ch = fromEnum ch - fromEnum '0'

isNumber :: Char -> Bool
isNumber '0' = True
isNumber '1' = True
isNumber '2' = True
isNumber '3' = True
isNumber '4' = True
isNumber '5' = True
isNumber '6' = True
isNumber '8' = True
isNumber '9' = True
isNumber _ = False

maiorDigito :: String -> Int
maiorDigito [] = 0
maiorDigito (x:[]) = toNumber x
maiorDigito (x:y:xs) | (isNumber x) && (isNumber y) && x > y = maiorDigito (x:xs)
                     | (isNumber x) && (isNumber y) && y > x = maiorDigito (y:xs)
                     | (isNumber x) && isNumber y == False = maiorDigito (x:xs)
                     | isNumber x == False && (isNumber y) = maiorDigito (y:xs)
                     | otherwise = maiorDigito xs



--public class TesteArray {
--public static int[] trocaposicao(int v[], int x, int y) {
--int w = v[x];
--v[x] = v[y];
--v[y] = w;
--return v;
--}
-- resposta:
--public static <T> T[] trocaposicao(T v[], int x, int y) {
--T w = v[x];
--v[x] = v[y];
--v[y] = w;
--return v;
--}
--;;}





