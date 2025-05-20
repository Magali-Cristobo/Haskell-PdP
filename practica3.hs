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

-- mapOS::(a -> b) -> [a] -> [b]
-- mapOS f [] = []
-- mapOS f (x:xs) = f x : mapOS f xs

-- map2::(a -> b) -> [[a]] -> [[b]]
-- map2 f [ ] = []
-- map2 f (xs:xss) = ((map f xs):(map2 f xss))  
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

zipWithB:: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithB f [] ys = []
zipWithB f (x:xs) [ ] = []
zipWithB f (x:xs) (y:ys) = ((f x y): (zipWithB f xs ys))

-- all si todos los elementos de la lista cumplen un criterio 
-- all :: (a -> Bool) -> [a] -> Bool
-- all _ [] = True
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
sumaInt::Int -> Int -> Int
sumaInt a b = a + b

sumaIntB::(Int, Int) -> Int
sumaIntB (a, b) =  a + b

divInt::Int -> Int -> Int
divInt a b = a `div` b

divIntB:: (Int, Int) -> Int
divIntB (a, b) = a `div` b

-- 8ii Definir --usando funciones currificadas anteriores-- a las funciones sucesor de un entero, predecesor de un entero, mitad de un entero y dosVeces la aplicación de una función.
sucesor:: Int -> Int
sucesor a = sumaInt a 1

-- otra forma
-- predecesor:: Int -> Int
-- predecesor 1 = 0
-- predecesor n = sumaInt 0 n-1

predecesor:: Int -> Int
predecesor = sumaInt (-1)

mitad::Int -> Int
mitad = divInt `flip` 2
-- mitad n = divInt n 2
-- mitad n = divInt  `flip` 2 n
-- mitad = divInt `flip` 2

dosVeces :: (a -> a) -> a -> a
dosVeces f  = f . f
-- dosVeces f  x = f (f x) 



-- 8 iii Definir cuatroVeces usando dosVeces.
cuatroVeces:: (a -> a) -> a -> a
cuatroVeces = dosVeces . dosVeces

-- cuatroVeces f x = dosVeces f (dosVeces f x)

-- cuatroVeces f = dosVeces (\ x -> (dosVeces f x)  ) 
-- cuatroVeces f = dosVeces (dosVeces f ) 
-- -- cuatroVeces = dosVeces . dosVeces



-- 9i Programar la función no currificada separar, que dada una condición y una lista devuelva un par de listas donde la primera esté conformada por aquellos elementos de la lista original que cumplan con la condición, y la segunda, por aquellos que no la cumplen.
separar:: (a -> Bool) -> [a] -> ([a], [a])
separar c [] = ([], [])
separar c (x:xs) = if (c x ) then (([x] ++ fst(separar c xs)), snd(separar c xs)) else (fst(separar c xs), ([x] ++ snd(separar c xs)))

-- separar:: (a -> Bool, [a]) -> ([a], [a])
-- separar (c, []) = ([],[])
-- separar (c, x:xs)
--   | c x    = (x:si, no)
--   | otherwise = (si, x:no)
--   where (si, no) = separar (c, xs)


-- 9ii Programar una versión currificada de separar usando filter.
separarF:: (a -> Bool) -> [a] -> ([a], [a])
separarF c [] = ([], [])
separarF c xs = (filter c xs, filter( \x -> not (c x)) xs) 
-- separarF c xs = (filter c xs, filter(not . c ) xs) 

-- 9 iii
mayoria:: (Num a, Ord a) => a -> [[a]] -> [[a]]
mayoria n = filter (\xs -> length (filter (>n) xs) > length (filter (<=n) xs))

-- mayoria n = filter (\xs -> length (filter (\x -> x >n) xs) > length (filter ((\x -> x <= n)) xs))
-- mayoria n = filter (\xs -> length (filter ((<) n) xs) > length (filter ((\x -> x <= n)) xs))

-- 10i Programar una función de orden superior paraCada que dados dos números (índice inicial y final), un valor inicial de un dato y una función que dado un dato y un índice devuelve un dato, devuelva la aplicación sucesiva de la función dada sobre el dato inicial y cada uno de los valores desde el índice inicial hasta el final.
paraCada:: (a -> Int -> a) -> Int -> Int -> a -> a
paraCada f i j d = if (i == j ) then d else paraCada f (i + 1) j (f d i)

--10 ii Programar una función todos que dada una lista de elementos y una condición sobre los elementos devuelva si todos los elementos de la lista cumplen con la condición. Usar paraCada, long y term. En Haskell se llama all.
todos :: (a -> Bool) -> [a] -> Bool
todos c xs = paraCada (\ x i -> x && c (term xs i)) 0 (long xs) True

term :: [a] -> Int -> a
term (a:_) 0 = a 
term (_:as) n = term as (n - 1)

long :: [a] -> Int
long [] = 0
long (a:as) = 1 + long (as)

ninguno :: (a -> Bool) -> [a] -> Bool
ninguno c xs = todos (\ x -> not (c x)) xs

igLong:: [[a]] -> Bool
igLong [] = True
igLong xss = todos (\xs -> (long xs) == long (ultimo xss)) xss

-- 11i Definir la función while, que dado un valor, una función que represente una condición y una función de transformación, devuelva la aplicación sucesiva de la función de transformación sobre el valor dado mientras se cumpla la función-condición dada. Si el valor inicial cumple la función-condición, ese valor deberá ser el devuelto.
while::(a -> Bool) -> (a -> a) -> a -> a
while c f n = if c n then (while c f (f n)) else n

--11 ii Definir la función until, que dado un valor, una función que represente una condición y una función de transformación, devuelva la aplicación sucesiva de la función de transformación sobre el valor dado hasta que se cumpla la función-condición dada. La función de transformación debe aplicarse una vez como mínimo.
untilB:: (a -> Bool) -> (a -> a) -> a -> a
untilB c f n = if not(c n) then n else (untilB c f (f n))

--11 iii definir ultimo usando while
ultimoB:: [a] -> a -- tenemos que quedarnos con la cabeza y el resto si o si, usamos una tupla.
ultimoB xs = snd (while (\ (xs, s) -> not (null (tail xs))) (\ (xs , s) -> (tail xs, head (tail xs))) (xs, head xs))

--11 iv Definir long, append, sumaLista y genLista usando while. Comparar estas funciones con las equivalentes realizadas en un lenguaje imperativo usando la estructura de control de iteración.
longB:: [a] -> Int
longB xs = snd (while (\ (xs, s) -> not (null xs)) (\ (xs, s) -> (tail xs, s+1)) (xs, 0))

appendB:: [a] -> [a] -> [a]
appendB xs ys = fst (while (\ (xs, ys) -> not (null ys)) (\ (xs, ys) -> (((head (ys)):xs), tail ys)) (ys, xs))

sumaLista:: [Int] -> Int
sumaLista xs = snd (while (\ (xs, _) ->  not (null xs)) (\ (xs, c) -> (tail xs, c + head xs)) (xs, 0))

genListaB:: (a -> a) -> Int -> a -> [a]
genListaB f n d = fst (while (\ (x, l) -> 0 < l ) (\ (x, l) -> (x ++ [f (last x)], l-1)) ([d], n))

-- 12i Definir una función currificada map, que dada una función y una lista como argumentos, devuelve otra lista que es el resultado de aplicar la función original (elemento a elemento) a cada uno de los elementos de la lista original.
mapOS::(a -> b) -> [a] -> [b]
mapOS f [] = []
mapOS f (x:xs) = f x : mapOS f xs

mapn:: ((a -> b), [a]) -> [b]
mapn (f , []) = []
mapn (f , (x:xs)) = f x : mapn (f, xs)
-- mapn (f, xs) = map f xs usando map

--13i Definir una función no currificada mapn2, que aplique una función a todos los elementos de todas las listas de una lista de listas.
mapn2:: ((a -> b ), [[a]]) -> [[b]]
mapn2 (f, []) = []
mapn2 (f, xss) = map (\ xs -> map f xs) xss
-- mapn2 (f, xss) = map (map f) xss

--13 ii) Definir una función map2 como la función currificada de mapn2 usando map.
map2::(a -> b) -> [[a]] -> [[b]]
-- map2 f [] = []
-- map2 f xss = map (\ xs -> map f xs) xss
-- map2 f = map (\ xs-> map f xs) 
map2 f = map (map f)

--13 iii) Definir otra versión de map2 usando la función infija o.
-- map2 = map <.> map 

-- 14 Definir una función mapArb, una versión de map que en lugar de recibir una lista, recibe un elemento de tipo ArbBin (árbol binario).
data ArbBin a = Hoja a | Nodo a (ArbBin a) (ArbBin a) deriving Show

mapArb:: (a -> b) -> ArbBin a -> ArbBin b
mapArb f (Hoja n) = Hoja(f n)
mapArb f (Nodo n si sd) = Nodo (f n) (mapArb f si) (mapArb f sd)

-- 15.	i) Definir una función mapo, una versión de map que toma una función de dos argumentos y una lista de pares de valores, y devuelve la lista de aplicaciones de la función a cada par.  ¿Qué se puede concluir?
mapo::(a -> b -> c) -> [(a, b)] -> [c]
mapo f [] = []
mapo f (x:xs) = f (fst x) (snd x) : mapo f xs
-- mapo f pares = map (\(x, y) -> f x y) pares

-- que se puede concluir??