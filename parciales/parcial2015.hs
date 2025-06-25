-- 1)	i)	Programar una función mult3 con recursión explícita, que dado un número entero y una lista de números enteros, retorne todos aquellos elementos 
-- pertenecientes a la lista (preservando el orden en que aparecen) que sean mayores al número entero dado y que se encuentren en las posiciones de índice que sea 
-- de resto 1 módulo 3. Si la primera posición de la lista corresponde al índice 0, la segunda posición al índice 1, y así, se hace referencia a los elementos de la 
-- posición 1, 4, 7, 10, etc.
mult3::Int -> [Int] -> [Int]
mult3 n xs = mult3b n 0 xs

mult3b::Int -> Int -> [Int] -> [Int]
mult3b n i [] = []
mult3b n i (x:xs)
    | x > n && mod i 3 == 1 = (x:mult3b n (i + 1) xs)
    | otherwise = mult3b n (i + 1) xs

mult3bb n xs = go 0 xs
    where
        go _ [] = []
        go i (x:xs)
            | x  > n && mod i 3 == 1 =  x: go (i + 1) xs
            | otherwise = go (i + 1) xs 

-- ii)	Idem i), pero utilizando la función foldr.

mult3F:: Int -> [Int] -> [Int]
mult3F n xs = snd (foldr (\ x (i,acc)-> if x > n && mod i 3 == 1 then ((i - 1), x:acc) else (i - 1, acc)) (length xs - 1 , []) xs)

-- alternativa con zip
-- mult3F :: Int -> [Int] -> [Int]
-- mult3F n xs = foldr (\(i,x) acc -> if x > n && i `mod` 3 == 1 then x:acc else acc) [] (zip [0..] xs)

-- 2)		Dada la siguiente expresión...

-- head [(x,y)| x <- [6,3,9], y <- [9,0,3,2], x > y]

-- ... evaluarla utilizando modo de evaluación eager por un lado y normal order por otro (la explicación del modo de evaluación eager puede ser menos detallada).

{-
en el modo eager tenemos que evaluar todos los parametros antes de aplicar la funcion, es decir que se construiria toda la lista
y quedaria [(3,0),(9,3)]
el resultado es (3,0)
en el modo normal order solo evaluamos el parametro hasta el primer constructor, es decir que head se evaluaria asi> head ((3,0): [(x,y)| x <- [9], y <- [3,2], x > y])
resultado -> (3,0)
-}
-- 3)i)	Definir la función de orden superior shuffle que dadas dos listas de elementos del mismo tipo y un criterio de “mezlado” de las dos listas, retorne una 
-- lista resultante que contenga todos los elementos unidos de las dos listas dadas donde siempre se cumple que si un elemento A aparece antes que otro B en una 
-- de las dos listas, el elemento A aparecerá antes que B en la lista resultante. El criterio de mezclado indica para un instante dado que dadas las longitudes 
-- totales de las dos listas originales, los contenidos de las cabezas de las dos listas, y las posiciones que poseen las dos cabezas en la lista respectiva original, 
-- si se deberá extraer la cabeza de la primera lista o de la segunda para agregarla al final de la lista resultante. Si alguna lista se llegara a vaciar y la otra no, 
-- automáticamente todos los elementos de la lista no vacía se agregan en ese orden al final de la lista resultante.
shuffle:: [a] -> [a] -> (Int -> Int -> a -> a -> Int -> Int -> Bool) -> [a]
shuffle xs ys f = shuffleb (length xs) (length ys) 0 0 xs ys f

shuffleb:: Int -> Int -> Int -> Int -> [a] -> [a] -> (Int -> Int -> a -> a -> Int -> Int -> Bool) -> [a]
shuffleb _ _ _ _ [] [] _= []
shuffleb _ _ _ _ [] ys _ = ys
shuffleb _ _ _ _ xs []  _= xs
shuffleb l1 l2 i j (x:xs) (y:ys) f = if (f l1 l2 x y i j) then (x: (shuffleb l1 l2 (i + 1) j xs (y:ys) f)) else  (y: (shuffleb l1 l2 i (j + 1) (x:xs) ys f))

agAtras:: [a] -> a -> [a]
agAtras (y:[]) x = (y:[x])
agAtras (y:ys) x = (y:agAtras ys x)

-- ii)	Definir la función intercalar, que dadas dos listas, retorne una lista que tenga los elementos intercalados de las dos listas dadas. Usar shuffle.
intercalar:: [a] -> [a] -> [a]
intercalar xs ys = shuffle xs ys (\ l1 l2 x y i j -> even (i + j))

-- iii)	Definir la función merge, que dadas dos listas ordenadas, retorne una lista ordenada que sea el merge de las dos listas dadas. Usar shuffle.
merge::Ord a => [a] -> [a] -> [a]
merge xs ys = shuffle xs ys (\ _ _ x y _ _ -> (x <= y))

-- iv)	Idem ii), pero cuando una lista llegue a una cantidad dada de elementos, todos los elementos de esa lista se extraerán primero para agregarlos a la lista 
-- resultante.
intercalarb:: [a] -> [a] -> Int -> [a]
-- no salio :( 

-- 4)	Expresar el tipo de datos más general (justificando lo más detalladamente posible) de la siguiente expresión:
	-- foldr head (head [])
{-
foldr: (a -> b -> b) -> b -> [a] -> b
head:: [a] -> a
(head []) :: [a] -> [a] -> a
tratamos de unificar el dominio de foldr con el tipo de head
dominio  (x -> y -> y), imagen y-> [x] -> y
sustituciones
x/ [a]
y/ a
devolvemos la imagen de foldr a -> [[a]] -> a
foldr head::  a -> [[a]] -> a 
foldr head (head []) :: [[a]] -> a
-}