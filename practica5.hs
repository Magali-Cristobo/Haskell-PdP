import Data.List (permutations)

-- 1.	i) Definir las funciones dividirPorDos y dividirPorTres, que dado un número entero, lo divide por dos o tres respectivamente. Si el número entero no es 
-- divisible por el número propio de la función, deberá retornar un valor “vacío/inválido”.

dividirPorDos::Int -> Maybe Int
dividirPorDos n = secuencializar (divisible n 2) (\ x -> return (div x 2))
-- dividirPorDos n = divisible n 2 >>= \x -> return (div x 2)

dividirPorTres::Int -> Maybe Int
dividirPorTres n = secuencializar (divisible n 3) (\ x -> return (div x 3))
-- dividirPorTres n = divisible n 3 >>= \x -> return (div x 3)

secuencializar (Just n) f = f n 
secuencializar (Nothing) f = Nothing

divisible::Int -> Int -> Maybe Int
divisible n m = if ((mod n m) == 0) then (Just n) else Nothing


-- sin crear divisible ni secuencializar usando el bind
-- dividirPorDos::Int -> Maybe Int
-- dividirPorDos n = if mod n 2 == 0 then Just n >>= (\ x -> return (div x 2)) else Nothing

-- ii) Definir las funciones dividirPorDieciocho y dividirPorVeinticuatro a partir de las funciones del punto i). Realizar las definiciones 
-- sin utilizar y utilizando la función (>>=).
dividirPorDieciocho::Int -> Maybe Int
dividirPorDieciocho n = dividirPorDos n >>= (\x -> dividirPorTres x >>=  (\ y -> dividirPorTres y >>= \z -> return z))
-- dividirPorDieciocho n = secuencializar (dividirPorDos n) (\ x -> secuencializar (dividirPorTres x) (\y -> secuencializar(dividirPorTres y)(\z -> return z)))

dividirPorVeinticuatro::Int -> Maybe Int
dividirPorVeinticuatro n = dividirPorDos n >>= (\x -> dividirPorDos x >>=  (\ y -> dividirPorDos y >>= (\z -> dividirPorDos z >>= \w -> return w)))

-- Idem punto ii), pero utilizando la notación do.
dividirPorDieciocho' n = do { x <- dividirPorDos n; y <- dividirPorTres x; z <- dividirPorTres y; return z}
dividirPorVeinticuatro' n = do { x <- dividirPorDos n; y <- dividirPorDos x; w <- dividirPorDos y; z <- dividirPorTres w; return z}

-- 2. i) Generar cuadradoMagico, que consiste en diversas combinaciones de 9 números distintos entre 1 y 9 dispuestos en una matriz de 3x3 tales que la suma de 
-- todos los valores en las filas, en las columnas y en las dos diagonales principales dé el mismo valor para todos los casos. Usar listas por comprensión.
cuadradoMagico::[[[Int]]] -- porque es una lista de matrices
cuadradoMagico = map toMatriz3x3 generarCombinaciones

generarCombinaciones::[[Int]]
generarCombinaciones = [cuadrado | cuadrado <- permutations[1..9], esMagico cuadrado]

esMagico:: [Int] -> Bool
esMagico [a,b,c,d,e,f,g,h,i] = a+b+c == 15 && d+e+f == 15 && g+h+i == 15 && a+d+g == 15 && b+e+h ==15 && c+f+i == 15 && a+e+i == 15 && c+e+g == 15 
esMagico _ = False

toMatriz3x3 :: [Int] -> [[Int]]
toMatriz3x3 [a,b,c,d,e,f,g,h,i] = [[a,b,c], [d,e,f], [g,h,i]]
toMatriz3x3 _ = error "La lista no tiene 9 elementos"

--  ii) Idem punto i) pero utilizando la notación do.
-- cuadradoMagico :: [[Int]]
-- cuadradoMagico = do
--   cuadrado <- permutations [1..9]
--   if esMagico cuadrado
--     then return cuadrado
--     else []

-- 3 Definir la función resultados, que dado un estado inicial y un State, retorne el stream de todos los resultados de la aplicación sucesiva del State en el mismo 
-- orden en que se fueron aplicando.
