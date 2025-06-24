-- 1)	i)	Programar una función multiplos que dada una lista y un número entero positivo retorne todos aquellos elementos de la lista dada que se 
-- encuentren en las posiciones que sean múltiplos del número positivo dado. El primer elemento de la lista corresponderá a la posición 0.
multiplos:: [a] -> Int -> [a]
multiplos [] n = []
multiplos xs n = [ xs !! i | i <- [0..length xs-1], mod i n == 0] -- !! es ineficiente para listas largas, recorre desde el inicio siempre
-- multiplos xs n = [x | (i, x) <- zip [0..] xs, mod i n == 0] -- no usa !!, es mejor

-- multiplos :: [a] -> Int -> [a]
-- multiplos xs n = aux xs 0
--   where
--     aux [] _ = []
--     aux (x:xs) i = if mod i n == 0 then x : aux xs (i+1) else aux xs (i+1)

-- ii)	Realizar la traza de evaluación mediante la técnica eager por un lado y normal order por otro, de la siguiente expresión:
	-- take 2 (multiplos 3 [7,9,0,5,2,3,4,6])
{-
definicion de take 
take:: [a] -> Int -> a
take xs 0  = xs
take (x:xs) n = (x: take (n-1) xs)
Con eager necesitamos evaluar todos los parametros de take (2 y el llamado a multiplos)
multiplos [7,9,0,5,2,3,4,6] 3 -> [7, 5, 4]
luego aplica take 2 [7,5,4] = [7,5]
en normal order no es necesario evaluar todo multiplos
primer elemento zip [0..] [7,9,..] -> (0, 7), como es multiplo de 3 se agrega 7 a la lista
segundo elemento zip [1..] [9..] -> (1,9) , no se agrega el 9
tercer elemento zip [2..] [0,5..] -> (2,0), no se agrega el 0
cuarto elemento zip [3..] [5,2.. ] -> (2, 5), 3 es multiplo de 3, se agrega el 5
como ahi ya tiene dos elementos termina el take 2
(no me convence mucho esto)
-}

-- 2)	i)	Programar una función de orden superior paraCada que dados dos números (índice inicial y final), un valor inicial de un dato y una función que dado un 
-- dato y un índice devuelve un dato, devuelva la aplicación sucesiva de la función dada sobre el dato inicial y cada uno de los valores desde el índice inicial 
-- hasta el final. Dar una versión usando foldr, otra usando foldl y otra sin usar ninguna de estas dos funciones.
paraCada:: (a -> Int -> a) -> Int -> Int -> a -> a
paraCada f i j d = if (i > j) then d else paraCada f (i + 1) j (f d i)

paraCadaFR:: (a -> Int -> a) -> Int -> Int -> a -> a
paraCadaFR f i j d = foldr (\ x acc -> f acc x) d [i..j]

paraCadaFL::(a -> Int -> a) -> Int -> Int -> a -> a
paraCadaFL f i j d = foldl (\ acc x -> f acc x) d [i..j]

-- ii)	Programar una función todos que dada una lista de elementos y una condición sobre los elementos devuelva si todos los elementos de la lista cumplen con la 
-- condición. Usar paraCada, length y (!!).
todos::(a -> Bool) -> [a] -> Bool
todos c [] = False
todos c xs = paraCada (\ x acc -> x && c (xs !! acc)) 0 (length xs-1) True

-- iii)	Programar una función ninguno que dada una lista de elementos y una condición sobre los elementos devuelva si ninguno de los elementos de la lista cumple con 
-- la condición. Usar todos y (.).
ninguno::(a -> Bool) -> [a] -> Bool
-- ninguno c xs = todos (\ x -> not (c x)) xs
ninguno c  = todos (not . c) 

-- iv)	Programar una función igLong, que dada una lista de listas, diga si todas las sublistas tienen igual longitud. Asumir que la lista de listas 
-- no debe ser vacía. Usar todos, length y last.
igLong:: [[a]] -> Bool
igLong [[]] = False
igLong xss = todos (\ xs -> (length xs) == length(last xss)) xss

-- 3)	Expresar el tipo de datos más general (justificando lo más detalladamente posible) de la siguiente expresión:
	-- foldr map ([]:[])
{-
tipos:
foldr:: (a -> b -> b) -> b -> [a] -> b
map :: ( a -> b) -> [a] -> [b]
([]:[]):: ([[a]])

1- Tenemos que tratar de unificar el dominio de foldr con el tipo de map. El dominio de foldr es (x -> y -> y)
Las sustituciones que vamos a realizar son:
x/ (a -> b)
y/ [a]
y/ [b]
a = b
y debemos retornar la imagen de foldr que es (y -> [x] -> y), reemplazando queda ([a] -> [(a -> a)] -> [a])
entonces foldr map :: [a] -> [(a -> a)] -> [a]]
ahora tenemos que unificar el dominio de foldr map con el tipo de ([]:[])
el dominio es [x] -> [(x -> x)] -> [x]
y el tipo es [[a]]
sustituciones:
x/ [a]
y devolvemos la imagen foldr map que es [(x->x)] -> [x]
con la sustitucion quedara [([a] -> [a])] -> [[a]]
-}