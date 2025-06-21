-- 1)	i)	Definir una lista de enteros primos que representa la lista infinita de todos los números naturales primos. 
-- Utilizar (y programar también) una función esPrimo con recursión explícita.
listaPrimos:: [Int]
listaPrimos = buscarPrimosDesde 2

buscarPrimosDesde:: Int -> [Int]
buscarPrimosDesde n = if esPrimo n then (n: buscarPrimosDesde (n + 1)) else buscarPrimosDesde (n + 1) 

esPrimo :: Int -> Bool
esPrimo n
  | n <= 1    = False
  | otherwise = not (tieneDivisor n 2)

tieneDivisor :: Int -> Int -> Bool
tieneDivisor n d
  | d >= n         = False
  | n `mod` d == 0 = True
  | otherwise      = tieneDivisor n (d + 1)

-- ii)	Idem anterior, pero utilizando listas por comprensión.
-- esPrimo n = (length (divisores n)) == 0
-- esPrimo n = if (length (filter (\ x-> mod n x == 0) [2..n-1])) == 0 then True else False

divisores:: Int -> [Int]
divisores n = [ x | x <- [2..n-1], mod n x == 0] 



-- 3)i)	Definir utilizando type el tipo de datos Matriz de elementos de un tipo determinado, a partir del tipo lista.
type Matriz a = [[a]]

-- ii)	Definir la función esValida, que dada una matriz, indique si se encuentra correctamente construida. Utilizar foldr.
esValida::Matriz a -> Bool
esValida [] = True
esValida (mat) = let largo = length (head mat) in foldr (\ fila acc -> ((length fila) == largo) && acc) True mat

-- iii)	Definir la función de orden superior calcular que dada una matriz, una primera función que dados todos los elementos de cada 
-- fila dé un resultado-fila, una segunda función que dados todos los elementos de cada columna dé un resultado-columna y una tercera 
-- función que dadas las dos listas de todos los resultados-fila y todos los resultados-columna, dé el resultado final, 
-- devuelva el resultado final para la matriz dada.
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []  -- si alguna fila está vacía, terminamos
transpose xs = map head xs : transpose (map tail xs) -- primero agarra la cabeza de todas las listas y sigue con el resto

calcular:: Matriz a -> ( [a] -> b) -> ([a] ->b) -> ([b] -> [b] -> c) -> c
calcular mat f g h = h filas cols
    where 
        filas = [f fila | fila <- mat]
        cols =  [g col |  col <- (transpose mat)]

-- calcular mat f g h = 
--     let resFilas = map f mat
--         resCols = map g (transpose  mat)
--     in h resFilas resCols


-- iv)	Definir la función esSimetrica, que dada una matriz cuadrada, retorne si es simétrica. Utilizar la función calcular.
esSimetrica::Matriz a -> Bool
-- esSimetrica mat = calcular mat (\ ) (\) (\ x y -> x == y )