-- previo a la practica
sacaPares::[Int] -> [Int]
sacaPares [] = []
sacaPares (x:xs) = if(even x) then (sacaPares xs) else (x:(sacaPares xs))

-- version con funciones de orden superior
-- filterOS:: (a -> Bool) -> [a] -> [a]
-- filterOS f [] = []
-- filterOS f (x:xs)  = if(f x) then (x:(filterOS f xs )) else (filterOS f xs )

-- otra forma 
{-
filter c (x:xs)
    | c x = x: filter c xs
    | otherwise = filter condicion xs

-}
sacaParesOS:: [Int] -> [Int]
-- sacaParesOS = filterOS (\x -> odd x)
sacaParesOS = filterOS odd 

mapOS::(a -> b) -> [a] -> [b]
mapOS f [] = []
mapOS f (x:xs) = f x : mapOS f xs

map2::(a -> b) -> [[a]] -> [[b]]
map2 f [ ] = []
map2 f (xs:xss) = ((map f xs):(map2 f xss))  
-- con transiciones lambda 
-- map2 f xss = map (\ xs-> map f xs) xss
-- map2 f = map (\ xs-> map f xs) 

sumatoria :: [Int] -> Int
sumatoria xs = foldr (\x r -> x + r) 0 xs
--sumatoria xs = foldr (\x r -> (+) x r) 0 xs
--sumatoria xs = foldr (\x -> (+) x) 0 xs
--sumatoria xs = foldr (+) 0 xs
-- sumatoria = foldr (+) 0


foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f x [] = x
foldR f x (y:ys) = f y (foldR f x ys)

foldL :: (b -> a -> b) -> b -> [a] -> b
foldL f x [] = x
foldL f x (y:ys) = foldL f (f x y) ys


-- all si todos los elementos de la lista cumplen un criterio 
-- all :: (a -> Bool) -> [a] - Bool
-- all _ [] = []
-- all condicion (x:xs) = condicion x && all condicion xs

hacerNVeces 0 f valor = valor
hacerNVeces n f valor | n > 0 = hacerNVeces (n-1) f (f valor)

-- 1 Definir una función esCerrada, que dada una lista y una función binaria (que toma un par de elementos del tipo de la lista y devuelve un elemento 
-- del tipo de la lista) devuelve si la función es cerrada respecto a la lista de entrada (o sea, si la imagen de la función 
--tomando como dominio la lista-- está incluída en esta lista “dominio”).

esCerrada:: Eq a => [a] -> (a -> a  -> a) -> Bool
esCerrada xs f = and [ f x y `elem` xs | x <- xs, y <- xs]

-- 2i Definir una función maxf, que dada una función con dominio e imagen en un tipo numérico ordenado y una lista (de tipo dominio de la función dada), devuelve el valor máximo de la imagen alcanzada por los elementos de la lista.
-- maxf:: Ord a => (a -> a) -> a -> [a] -> a
-- maxf f x [] = x -- si la lista esta vacia devolvemos el elemento actual
-- maxf f x (a:as) = if ((f a) > (f x)) then (maxf f a as) else (maxf f x as)
maxf :: (Ord b) => (a -> b) -> [a] -> b
maxf f xs = maximum (map f xs)

-- 2ii
minf:: (Num b, Ord b) => (a -> b) -> [a] -> b
minf f xs = negate (maxf (\x -> negate (f x)) xs)
-- min f(x) = -max (-f(x))


-- 3i Definir una función supf, que dadas dos funciones del mismo dominio e imagen, devuelva la función máximo puntual (para todo elemento del dominio devuelve el máximo de las aplicaciones de las dos funciones).
supf::  Ord b => (a -> b) -> (a -> b) -> (a -> b)
supf f g =  \x -> max (f x) (g x)

-- 3ii Definir inff (ídem anterior, pero devolviendo el mínimo) usando supf
inff::  (Num b, Ord b) => (a -> b) -> (a -> b) -> (a -> b)
inff f g = \x -> negate (supf (\y -> negate (f y)) (\y -> negate (g y)) x)

-- 4i Definir una función genLista que genere una lista de una cantidad dada de elementos a partir de un elemento inicial dado y de una función dada que represente 
-- la función de “incremento” entre los elementos de la lista (que dado un elemento que pertenezca a la lista devuelva el elemento siguiente en la misma lista, 
-- si es que existe por la longitud).
genLista:: (a -> a) -> Int -> a -> [a]
genLista f 0 x = []
genLista f n x =  x : genLista f (n - 1) (f x)

--4ii Definir « usando genLista.
(<->):: Int -> Int -> [Int]
x <-> n = genLista (+1) (2 * n + 1) (x - n)
-- considere que <-> era una lista centrada en un x, de longitud n

-- 5 Definir una función filter que depura una lista, dejando sólo aquellos elementos que cumplen con una condición dada. Esta condición está reflejada en una función booleana. Ej: filter even [1,2,3,4,5] ® [2,4]. En Haskell esta función no se aplica sobre listas, sino sobre elementos de un tipo perteneciente a la clase MonadZero, que incluye a las listas.
filterOS:: (a -> Bool) -> [a] -> [a]
filterOS f [] = []
filterOS f (x:xs)  = if(f x) then (x:(filterOS f xs )) else (filterOS f xs )

-- 6i Definir la función infija o de composición de funciones. En Haskell esta función está predefinida y se llama (.). ¿Qué restricciones habrá que hacerle respecto a la composición matemática?
infixr 9 <.>
(<.>):: (a -> b) -> (c -> a) -> c -> b
f <.> g = \x-> f(g x)

-- 6 ii Definir ultimo y segundo (devuelve el segundo elemento de una lista) usando la función o.
segundo:: [a] -> a
segundo = head <.> tail

ultimo:: [a] -> a
ultimo = head <.> reverse 
-- la restriccion es que la imagen de g debe estar incluida en el dominio de f

-- 7i Definir la función curry, que dada una función de dos argumentos, devuelve su equivalente currificada.  ¿Qué limitaciones debe tener esta función?
-- curry:: ((a -> b )-> c) -> (a -> b -> c)
-- curry f x y = f(x,y)
-- limitaciones: solo convierte funciones que reciben un par como único argumento en funciones curificadas.

-- 7ii Definir la función uncurry, que dada una función currificada de dos argumentos, devuelva su versión no currificada equivalente.
-- uncurry:: (a -> b -> c) -> ((a -> b) -> c)
-- uncurry f (x,y) = f x y

--  iii) ¿Qué diferencia fundamental existe entre una función currificada y su equivalente no currificada?  Mostrar algunos ejemplos en que la currificación resulte beneficiosa.  ¿Existe una manera única de currificar una función?
{-
Función no currificada:
Toma todos sus argumentos a la vez en forma de una tupla (o lista, o estructura similar)
f :: (Int, Int) -> Int
f (x,y) = x + y

Función currificada:
Toma un argumento a la vez, devolviendo otra función para el siguiente argumento, y así sucesivamente hasta tener todos
g :: Int -> Int -> Int
g x y = x + y
equivale a g :: Int -> (Int -> Int)

En la versión no currificada, llamás a la función con una tupla.

En la versión currificada, llamás a la función con un argumento, obtenés otra función esperando el siguiente argumento, y así sucesivamente.
La currificacion es beneficiosa para>
- Parcial aplicación de funciones
- Composición más sencilla
- Mayor modularidad y reutilización

Para funciones que toman varios argumentos, la currificación estándar es única: convertir f :: (a,b) -> c en a -> b -> c.
pero se pueden currificar funciones con más argumentos en diferentes formas, dependiendo del orden en que se aplquen los argumentos.
f :: a -> b -> c -> d
f :: (a, (b, c)) -> d

-}

--  iv) Discuta la posibilidad de definir una función curryN, que tome una función de un número arbitrario de argumentos (aunque siempre en formato de producto cartesiano de dos tipos) y devuelva su versión currificada.
-- no se me ocurre como hacerlo porque no sabemos la cantidad de parametros que vamos a tener, no podemos tener n parametros

--8i Definir las versiones currificadas y no currificadas suma de dos enteros y division entera de dos enteros.
