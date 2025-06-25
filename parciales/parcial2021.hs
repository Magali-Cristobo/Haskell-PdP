-- 1) i) Definir una función inferioresMayores, que dada una lista de listas de números (reflejando la idea de una matriz expresada como una lista de filas), 
-- retorne los elementos de la triangular inferior que sean mayores a un valor dado, usando recursión explícita y/o funciones de orden superior.

inferioresMayores:: (Num a, Ord a) => [[a]] -> a -> [a]
inferioresMayores matriz n = filter (\ x -> x > n) (triangularInferior matriz)
-- inferioresMayores matriz n = filter (> n) (triangularInferior matriz)

triangularInferior :: [[a]] -> [a]
triangularInferior matriz = concat (zipWith take [1..] matriz)

-- triangularInferior :: [[a]] -> [a]
-- triangularInferior = triangularAux 1
--   where
--     triangularAux _ [] = []
--     triangularAux n (fila:filas) = take n fila ++ triangularAux (n + 1) filas

-- triangularInferior :: [[a]] -> [a]
-- triangularInferior matriz =
--   [x | (i, fila) <- zip [1..] matriz, x <- take i fila]

-- triangularInferior :: [[a]] -> [a] -- con foldr
-- triangularInferior matriz = snd (foldr f (length matriz, []) (matriz))
--   where
--     f fila (n, acc) = (n - 1, take n fila ++ acc)

-- inferioresMayoresLPC :: [[Int]] -> Int -> [Int]
-- inferioresMayoresLPC xs n = [ x | x <- concat (triangularInferior xs), x > n]

-- triangularInferior :: [[Int]] -> [[Int]]
-- triangularInferior [] = []
-- triangularInferior xs = triangInferiorAux xs 1 (length (head xs))

-- triangInferiorAux :: [[Int]] -> Int -> Int -> [[Int]]
-- triangInferiorAux xs n long
--     | n > long = []
--     | otherwise = (drop n (head xs)):(triangInferiorAux (tail xs) (n + 1) long)

-- ii) Idem anterior, pero utilizando listas por comprensión (sin usar recursión explícita y evitando funciones de orden superior innecesarias).

inferioresMayoresLC :: [[Int]] -> Int -> [Int]
inferioresMayoresLC matriz n = [x | (fila, i) <- zip matriz [1..], x <- take i fila, x > n]

-- iii) Idem anterior, pero utilizando mónadas (sin usar recursión explícita y evitando funciones de orden superior y listas por comprensión innecesarias).
inferioresMayoresMon :: [[Int]] -> Int -> [Int]
inferioresMayoresMon matriz n = do
  (fila, i) <- zip matriz [1..]
  x <- take i fila
  if x > n then return x else []

-- 3) i) Definir el tipo de datos ListaConBase, que se caracteriza por ser una lista con todos los elementos del mismo tipo salvo el último, que puede ser 
-- eventualmente de otro tipo. No existen listas vacías de este tipo.
data ListaConBase a b = Lista [a] (Either a b )-- lo que pense que es que puede recibir toda la lista y el ultimo lo recibe por separado
-- data ListaConBase a b = Base b | Cons a (ListaConBase a b) deriving Show
--  ii) Definir la función foldListaConBase, que representa la versión análoga a la función foldr para el tipo ListaConBase. Definir esta función con parámetros adecuados.
foldListaConBase:: (a -> d -> d) -> (Either a b -> d) -> ListaConBase a b -> d
foldListaConBase f g (Lista xs ult) = foldr f (g ult) xs

-- foldListaConBase f g (Lista xs ult) = foldRec xs
--   where
--     foldRec []     = g ult
--     foldRec (y:ys) = f y (foldRec ys)

-- iii) Definir utilizando type el tipo ListaNoVacia(a) que representa a las listas no vacías, a partir del tipo ListaConBase.
type ListaNoVacia a = ListaConBase a a

-- iv)  Definir la función foldrLNV, de las listas no vacías que tenga un comportamiento idéntico a la función foldr del tipo [a] (con la excepción de que el tipo no acepta listas vacías). Usar foldListaConBase.
foldrLNV :: (a -> b -> b) -> (a -> b) -> ListaNoVacia a -> b
foldrLNV f g = foldListaConBase f (\ e -> case e of Right x -> g x)

-- 4) Expresar y justificar el tipo de datos más general inferido de la siguiente expresión:
	-- foldr map []
{-
definicion de foldr
foldr:: (a -> b -> b) -> b -> [a] -> b
map:: (a -> b) -> [a] -> [b]
[]:: [a]
tenemos que unificar el dominio de foldr (x -> y -> y) con el tipo de map, retornando la imagen de foldr (y -> [x] -> y) 
sustituciones:
x/ (a -> b)
y/ [a]
y/ [b]
entonces y:: [a]
foldr map:: [a] -> [(a -> a)] -> [[a]]
dominio de foldr map :: [x] -> [(x -> y)] -> [x]
hay que unificar el dominio [x] -> [(x -> y)] con el tipo [a]
sustitucion:
x/a
resultado final:
foldr map [] :: [(a -> a)] -> [a]

ejemplo de aplicacion:
foldr (\f acc -> map f [10,20] ++ acc) [] [(+1), (*2)]

-}
