	-- i)	Programar una función triang, que dada una lista de listas de enteros (representando una matriz cuadrada como lista de filas), 
    -- retorne los elementos correspondientes a la triangular inferior.
triang:: [[a]] -> [[a]]
triang mat =  triangb mat 1

triangb:: [[a]] -> Int -> [[a]]
triangb [] _ = []
triangb (m:mat) i = (take i m: triangb mat (i + 1) )

triangInfB:: [[a]] -> [[a]]
triangInfB mss = zipWith take [1..length mss] mss

-- 2)	i)	Definir la función de orden superior unshuffle que dada una lista de elementos de un tipo, una cantidad de listas de elementos a separar y 
-- un criterio de “separación” de la lista, retorne una lista de listas resultante que contenga todos los elementos separados de la lista dada donde siempre 
-- se cumple que si un elemento A aparece antes que otro B en la lista, el elemento A aparecerá antes que B en una de las listas resultantes, si es que ambos 
-- pertenecen a la misma lista. 
-- El criterio de separación indica para un instante dado que dada la longitud total de la lista original, el contenido de la cabeza 
-- de la lista, y la posición que posee la cabeza en la lista original, en cuál de las listas resultantes se deberá dejar el elemento extraído de la cabeza de la 
-- lista (en la 1, en la 2 y así hasta en la última). Si la lista se llegara a vaciar, el proceso finaliza.
unshuffle:: [a] -> Int -> (Int -> a -> Int -> Int) -> [[a]]
unshuffle [] _ _ = []
unshuffle _ 0 _ = []
unshuffle xs n f =  direccionar f length xs 0 (map) -- la idea es generar tantas listas como numeros???

direccionar::Int -> a -> Int -> Int -> (Int -> a -> Int -> Int) -> [[a]] -> [[a]] -- longitud de la lista, elemento actual, posicion, cant restantes
direccionar l actual pos rest f xss = (xss !! f l actual pos) ++ actual