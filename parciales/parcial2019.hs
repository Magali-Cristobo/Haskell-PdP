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

-- 2)		Dada las siguientes definiciones...

		x ** y = if (x == 0 || y == 0) then 0 else x * y
		lista = (0 : (map (+1) lista))

		-- ...evaluar la expresión foldr ((**) . (\y -> y-1)) 35 lista utilizando modo de evaluación eager por un lado y normal order por otro, explicando con mayor detalle el modo de evaluación normal order.
{-
En el modo de evaluacion eager se van a intentar evaluar todos los parametros antes de aplicar la funcion. En este caso el parametro lista es una lista infinita, por lo que el programa no va a terminar
En el caso de normal order se evalua lo "suficiente" para poder hacer pattern matching con la definicion de la funcion que se quiere aplicar
En este caso la funcion principal es foldr 
foldr:: (a -> b -> b) -> b -> [a] -> b
foldr f x [] = []
foldr f x (y:ys) = f y (foldr f x ys)  

Comenzamos con la aplicacion
foldr((**) . (\ y -> y - 1)) 35 (0: [1..])
las funciones siempre matchean con patrones variables, el 35 matchea con x que es una variable y la lista con el constructor (y:ys), entonces podemos hacer una aplicacion parcial
((**) . (\ y -> y - 1)) 0 (foldr  (**) . (\y -> y - 1) 35 (map (+1) lista))
podemos expresar la funcion compuesta como foldr (\x y -> (x - 1) ** y) 35 lista
((**) (\ y -> y -1 ) 0) (foldr  (**) . (\y -> y - 1) 35 (map (+1) lista))
(0 - 1) **  (foldr  (**) . (\y -> y - 1) 35 (map (+1) lista))
Como la funcion tiene pattern matching con dos variables, unifica con el llamado de foldr y no es necesario que lo calculemos, pero como se compara y con 0, debemos calcularlo
(foldr  (**) . (\y -> y - 1) 35 (1:[2..]))
((**) . (\ y -> y - 1)) 1 (foldr (**) . (\ y -> y - 1) 35 [2..])
(1-1) ** (foldr (**) . (\ y -> y - 1) 35 [2..])
unifica con **, pero como x es 0, no continua con el resto.
volviendo a  -> (0 - 1) **  (foldr  (**) . (\y -> y - 1) 35 (map (+1) lista))
              -> 1 ** 0 -> 0
-}


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
esSimetrica::Eq a => Matriz a -> Bool
esSimetrica mat = calcular mat id id (\ x y -> x == y )