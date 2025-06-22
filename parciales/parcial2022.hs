-- 1)	i)	Programar una función partes con recursión explícita, que dada una lista de elementos de un tipo variable -que representan conjuntos-,
--  retorne una lista de listas (el conjunto de conjuntos) que represente todos los subconjuntos del conjunto dado.
rev:: [a] -> [a]
rev xs = revAux xs []
  where
    revAux [] ys = ys
    revAux (x:xs) ys = revAux xs (x:ys)

partes:: [a] -> [[a]]
-- partes [] = []
-- partes xs = (xs: (map (\x -> [x]) xs)) ++ (partes (tail xs))

-- partes xs = [] ++ ((xs):(partes (tail xs)))
partes [] = [[]]
partes (x:[]) = [[], [x]]
partes (x:xs) = partes xs ++ map (x:) (partes xs)
--  = let ps = partes xs in ps ++ map (x:) ps
-- map (\y -> x:y) ps
{-
conjunto {1,2,3}
partes {[], [1],[2], [3], [1,2],[2,3],[1,3], [1,2,3]}

-}

-- ii)	Idem i) utilizando la función foldr
partesFold:: [a] -> [[a]]
partesFold xs = foldr (\ x acc -> acc ++ map (x:) acc ) [[]] xs

-- foldr f acc [] = acc
-- foldr f acc (x:xs) = f x (foldr f acc xs) 

-- 2)		Dada la siguiente definición...
		-- q :: (a -> a) -> a
		-- q x = x (q x)

		-- ...evaluar la siguiente expresión utilizando modo de evaluación normal order, y explicar las diferencias más significativas con el modo de evaluación eager:
	-- head (q (\f xs -> if (null xs) then id else ((head xs):) . (f (tail xs)))
	    --  [6,7,1] [3,3])
-- 3)	i)	Dada una lista de listas de números que asemejan la forma de una matriz cuadrada (lista de filas donde cada fila corresponde a los 
-- contenidos de sus columnas, y donde todas las filas poseen la misma cantidad de elementos), se solicita realizar una función que obtenga la 
--  (triangular superior tomando como base a la diagonal secundaria). Utilizar foldr y/o map.
triangularSupSecundaria::[[a]] -> [[a]]

triangularSupSecundaria mat = tail (foldr (\fila acc -> drop (length acc - 1) fila : acc) [[]] mat) -- el tail es para sacar la []

	-- ii)	Idem i), pero utilizando listas por comprensión o mónadas.
triangularSuperiorb:: [[a]] -> [a]
triangularSuperiorb mat = [ x | (fila, i) <- zip mat (rev [1..(length mat)]) , x <- drop i fila]
-- revisar este
{-
[
[1,2,3,4],
[5,6,7,8],
[9,10,11,12],
[13,14,15,16]
]
devuelve
[[],[8],[11,12],[14,15,16]]
-}