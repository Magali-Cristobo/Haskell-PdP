-- 1)	i)	Definir una función pares, que dado un número entero positivo, devuelva una lista de pares de números enteros positivos, con el primer componente del 
-- par menor o igual que el segundo componente, cuya suma sea igual al número entero positivo dado. Por ejemplo:
-- 			pares 7 = [(1,6),(2,5),(3,4)]
-- 			pares 10 = [(1,9),(2,8),(3,7),(4,6),(5,5)]
-- 		Definirla utilizando recursión explícita de pila.
-- 	ii)	Idem anterior, pero utilizando mónadas.
pares:: Int -> [(Int, Int)]
pares n = paresb n 1 (n -1)

paresb:: Int -> Int -> Int -> [(Int, Int)]
paresb n x y = if (x < y) && ((x + y) == n) then ((x, y):paresb n (x + 1) (y - 1)) else if (x < y) then (paresb n (x + 1) (y - 1) ) else []

paresbb :: Int -> [(Int, Int)]
paresbb n = generar 1
  where
    generar a
      | a > n `div` 2 = []
      | otherwise     = (a, n - a) : generar (a + 1)

{-
pares 2 (1,1)
pares 3 (1,2)
pares 4 (1,3)
pares 5 (1,4) (2,3)
pares 6 (1,5) (2,4) 
pares 7 (1,6) (2,5) (3,4)

pares n = [(1, n-1),  (2, n-2), (3, n-3)...]
-}
-- ii) usando monadas
paresc:: Int -> [(Int, Int)]
paresc n = [(a, b) | a <- [1..n], b <- [a..n], a + b == n]

-- 3)i)	Definir el tipo de datos ArbolNRot, que representa un árbol n-ario donde los nodos poseen un valor de un tipo dado, y donde los arcos (rótulos)
--  que unen un nodo con cada subárbol también poseen un valor de eventualmente otro tipo dado. Considerar que el tipo puede ser paramétrico.
data ArbolNRot a b = Nodo a [(b,ArbolNRot a b)]
    
-- ii)	Definir la función de orden superior foldrANR para un árbol ArbolNRot en forma análoga a la función foldr de las listas (recursión de pila), 
-- y que posea parámetros adecuados que apliquen a este tipo de datos.
foldrANR :: (a -> [(b, r)] -> r) -> ArbolNRot a b -> r
foldrANR f (Nodo valor hijos) = 
  let hijosProcesados = [(r, foldrANR f subarbol) | (r, subarbol) <- hijos]
  in f valor hijosProcesados


-- foldrANR :: (a -> [(b, r)] -> r) -> ArbolNRot a b -> r
-- foldrANR f (Nodo valor hijos) =
--   f valor (procesarHijos hijos)
--   where
--     procesarHijos :: [(b, ArbolNRot a b)] -> [(b, r)]
--     procesarHijos [] = []
--     procesarHijos ((rótulo, subarbol):resto) =
--       let resultadoSubarbol = foldrANR f subarbol
--       in (rótulo, resultadoSubarbol) : procesarHijos resto


-- iii)	Definir la función rotulosRamas, que dado un árbol ArbolNRot con nodos de un tipo arbitrario y rótulos enteros, retorne una lista de enteros donde cada elemento entero se asocia a cada rama 
-- del árbol y corresponde a la suma de todos los rótulos que corresponden a la misma rama. Usar foldrANR.
rotulosRamas :: ArbolNRot a Int -> [Int]
rotulosRamas arb = foldrANR f arb
  where
    f _ [] = [0]  -- Si es una hoja, hay una única rama sin rótulos: suma = 0
    f _ hijos = concatMap sumarRama hijos

sumarRama :: (Int, [Int]) -> [Int]
sumarRama (rótulo, ramas) = map (rótulo +) ramas

-- rotulosRamas :: ArbolNRot a Int -> [Int]
-- rotulosRamas = foldrANR (\_ hijos ->
--   if null hijos
--     then [0]
--     else concatMap (\(r, ramas) -> map (r +) ramas) hijos
--   )