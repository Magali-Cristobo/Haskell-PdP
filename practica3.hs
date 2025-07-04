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
-- inff f g = (\x -> negate (supf (negate . f) (negate . g) x))

-- 4i Definir una función genLista que genere una lista de una cantidad dada de elementos a partir de un elemento inicial dado y de una función dada que represente 
-- la función de “incremento” entre los elementos de la lista (que dado un elemento que pertenezca a la lista devuelva el elemento siguiente en la misma lista, 
-- si es que existe por la longitud).
genLista:: (a -> a) -> Int -> a -> [a]
genLista f 0 x = []
genLista f n x =  x : genLista f (n - 1) (f x)

--4ii Definir « usando genLista.
-- (<->):: Int -> Int -> [Int]
-- x <-> n = genLista (+1) (2 * n + 1) (x - n)
-- considere que <-> era una lista centrada en un x, de longitud n

-- 5 Definir una función filter que depura una lista, dejando sólo aquellos elementos que cumplen con una condición dada. Esta condición está reflejada en una función booleana. Ej: filter even [1,2,3,4,5] ® [2,4]. En Haskell esta función no se aplica sobre listas, sino sobre elementos de un tipo perteneciente a la clase MonadZero, que incluye a las listas.
filterOS:: (a -> Bool) -> [a] -> [a]
filterOS f [] = []
filterOS f (x:xs)  = if(f x) then (x:(filterOS f xs )) else (filterOS f xs )

-- 6i Definir la función infija o de composición de funciones. En Haskell esta función está predefinida y se llama (.). ¿Qué restricciones habrá que hacerle respecto a la composición matemática?
infixr 9 <.>
(<.>):: (a -> b) -> (c -> a) -> c -> b
f <.> g = \x -> f(g x)

-- 6 ii Definir ultimo y segundo (devuelve el segundo elemento de una lista) usando la función o.
segundo:: [a] -> a
segundo = head <.> tail

ultimo:: [a] -> a
ultimo = head <.> reverse 
-- la restriccion es que la imagen de g debe estar incluida en el dominio de f

-- 7i Definir la función curry, que dada una función de dos argumentos, devuelve su equivalente currificada.  ¿Qué limitaciones debe tener esta función?
-- curry:: ((a, b )-> c) -> (a -> b -> c)
-- curry f x y = f (x,y)
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
-- ultimoB xs = snd (while (\ (xs, s) -> not (null (tail xs))) (\ (xs , s) -> (tail xs, head (tail xs))) (xs, head xs))
ultimoB xs = head(while (\ ys -> not (null (tail ys))) tail xs)

--11 iv Definir long, append, sumaLista y genLista usando while. Comparar estas funciones con las equivalentes realizadas en un lenguaje imperativo usando la estructura de control de iteración.
longB:: [a] -> Int
longB xs = snd (while (\ (xs, s) -> not (null xs)) (\ (xs, s) -> (tail xs, s+1)) (xs, 0))

appendB:: [a] -> [a] -> [a]
appendB xs ys = fst (while (\ (xs, ys) -> not (null ys)) (\ (xs, ys) -> (((head (ys)):xs), tail ys)) (ys, xs))

sumaLista:: [Int] -> Int
sumaLista xs = snd (while (\ (xs, _) ->  not (null xs)) (\ (xs, c) -> (tail xs, c + head xs)) (xs, 0))

genListaB:: (a -> a) -> Int -> a -> [a]
genListaB f n d = fst (while (\ (x, l) -> 0 < l ) (\ (x, l) -> (x ++ [f (last x)], l-1)) ([d], n-1))

-- 12i Definir una función currificada map, que dada una función y una lista como argumentos, devuelve otra lista que es el resultado de aplicar la función original (elemento a elemento) a cada uno de los elementos de la lista original.
mapOS::(a -> b) -> [a] -> [b]
mapOS f [] = []
mapOS f (x:xs) = f x : mapOS f xs

-- 12ii Definir la función mapn, como la versión no currificada de map.
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
-- mapo f xs = map (uncurry f) xs

-- se puede concluir en mapo f xs = map (uncurry f) xs
-- uncurry devuelve ((a -> b) -> c)

-- 15 ii) Definir una función mapo2, una versión de mapo que toma una función currificada de dos argumentos y dos listas (de igual longitud), y devuelve una 
-- lista de aplicaciones de la función a cada elemento correspondiente a las dos listas. Esta función en Haskell se llama zipWith.
mapo2:: (a -> b -> c) -> [a] -> [b] -> [c]
mapo2 f [] ys = []
mapo2 f (x:xs) [] = []
mapo2 f (x:xs) (y:ys) = f x y: mapo2 f xs ys

-- 15 iii) Modificar la función mapo2 anterior (si lo cree necesario) para que pueda ser usada por una función sumamat, que suma dos matrices y devuelve otra matriz. 
-- Asumir que las dos matrices de entrada tienen la misma cantidad de filas y de columnas. Explicitar además a qué tipo es equivalente el tipo matriz.
type Matriz a = [[a]]
mapom:: (a -> b -> c) -> [a] -> [b] -> [c]
mapom f [] ys = []
mapom f (x:xs) [] = []
mapom f (x:xs) (y:ys) = f x y: mapo2 f xs ys


sumamat:: Num a=> Matriz a -> Matriz a -> Matriz a
-- sumamat xss yss = mapo2(\ xs ys -> (mapo2 (\ x y -> x + y) xs ys)) xss yss
sumamat = mapo2(mapo2 (+)) 

-- 16 Si xs es una lista, la evaluación de map f (map g xs), xs requiere recorrer dos veces la lista xs. Simplificar la expresión de modo que sólo deba recorrérsela 
-- una vez, definiendo una función simplif, que reciba las dos funciones y la lista.

simplif:: (b -> c) -> (a -> b) -> [a] -> [c]
-- simplif _ _ [] = []
simplif f g = map (f . g) 

-- 17 Definir una función sigma que calcule la suma de una serie. Los parámetros son lower (límite inferior de la sumatoria), upper (límite superior) y 
-- una función que indique el término general en la serie. Usar las funciones sumaLista, « y map.
sigma:: (Int -> Int) -> Int -> Int -> Int
sigma f lower upper = sumaLista (map f lista ) where lista =  (<->) lower (upper - lower)

-- Definir una función pascal que devuelva en forma de listas el triángulo de Pascal (o de Tartaglia) hasta la altura pedida. No se permite el uso de números 
-- combinatorios. Usar ultimo, paresConsec (función que dada una lista devuelve todos los pares de elementos junto a su sucesor) y map.
-- Ejemplo de paresConsec: paresConsec [7,3,2,5] ® [(7,3),(3,2),(2,5)].
-- Ej: pascal 4 ® [ [1], [1,1], [1,2,1], [1,3,3,1], [1,4,6,4,1] ]
paresConsec:: [Int] -> [(Int, Int)]
paresConsec [] = []
paresConsec [_] = [] -- me habia olvidado de esto
paresConsec (x:(xs: [])) = [(x, xs)]
paresConsec (x:(xs: xss)) = [(x, xs)] ++ paresConsec (xs:xss)
-- paresConsec xs = zip xs (tail xs)

-- La función zip en Haskell toma dos listas y devuelve una lista de pares, donde el i-ésimo par contiene el i-ésimo elemento de cada lista. Si una lista es más corta, la función termina ahí
-- zip :: [a] -> [b] -> [(a, b)] 
-- zip (x:xs) (y:ys) = (x, y) : zip xs ys
-- zip _ _ = []

sumaPares::[(Int, Int)] -> [Int]
sumaPares [] = []
sumaPares (x:xs) = fst x + snd x : sumaPares xs

pascal:: Int -> [[Int]]
pascal 0 = [[1]]
-- pascal n =  anteriores ++ [[1] ++ (sumaPares ( paresConsec (ultimo anteriores))) ++ [1]] where anteriores = pascal (n - 1)
pascal n =  anteriores ++ [[1] ++ (map ( uncurry (+)) ( paresConsec (ultimo anteriores))) ++ [1]] where anteriores = pascal (n - 1)
-- map reemplazaria el suma pares utilizando uncurry de la funcion suma aplicada a todos los anteriores

-- otra forma
-- pascal n = anteriores ++ [nuevos] 
--     where 
--         anteriores = pascal (n - 1) 
--         ultima = ultimo anteriores
--         nuevos = [1] ++ sumaPares (paresConsec ultima) ++ [1]

-- con map
-- pascal :: Int -> [[Int]]
-- pascal 0 = [[1]]
-- pascal n = let anteriores = pascal (n - 1)
--                anterior = last anteriores
--                nueva = 1 : map (uncurry (+)) (paresConsec anterior) ++ [1]
--            in anteriores ++ [nueva]

-- 19 i) Definir la función de orden superior mapearF, que dada una lista de funciones y una lista (donde ambas tienen la misma cantidad de elementos), devuelva otra lista con cada 
-- resultado de la aplicación de cada función a su elemento correspondiente de la lista.
mapearF:: [(a -> b)] -> [ a ] -> [ b ]
mapearF _ [] = []
mapearF [] _ = []
mapearF (f:fs) (x:xs) = f x : (mapearF fs xs)
 -- se llama zipWith ($) supuestamente

-- 19 ii) Definir la función paresEnPosic, que dada una lista de enteros, devuelva si en cada posición i-ésima de la lista se encuentra el i-ésimo par natural positivo. Usar mapearF.
paresEnPosic:: [Int] -> Bool
paresEnPosic [] = False
paresEnPosic xs = all (\ x -> x == True) (mapearF [(==x)| x <- [0..(2*long xs)], even x] xs)
-- paresEnPosic xs = all id (mapearF [(==x) | x <- take (length xs) [2,4..]] xs)

-- 20 Decir cuál es el tipo más general de f (si es posible calcularlo) cuya definición es:
 -- i) f (x,y) = mapn (x,(x y))
 -- mapn recibe una funcion de a -> b, que seria x en este caso y un arreglo [a]. Entonces x tiene que ser del tipo (a -> [b]), y sera del tipo a (por la definicion de la funcion)
 -- entonces quedaria f :: ((a -> [b]), a) -> [b]

 -- ii) f (x,y,z) = mapn (x,(x y):z)
-- x:: a -> b
-- (x y):z :: [b], deberia ser [a] segun mapn
-- x y :: b
-- y::a
-- z:: [b]
-- quedaria f :: ((a -> c), a, [a]) -> [c]

-- es valido si mapn :: (a -> b, [a]) -> [b]

 -- iii) f = map map
-- map::(a -> b) -> [a] -> [b]
-- f g xss = map (\ xs -> map g xs) xss
-- g:: (a -> b)
-- xs:: [a]
-- xss:: [[a]]
-- sabemos que devuelve [[b]]

-- f:: (a -> b) -> [[a]] -> [[b]]
-- puse eso pero chatgpt me lo corrigio a [a -> b] porque no estan general la otra opcion, podrian ser varias funciones aplicadas no una sola

-- iv) f = curry `o` curry
-- como hay 2 currys no puede ser ((a, b )-> c), va a tener que tener dos tuplas para que tenga sentido
-- curry:: ((a, b ) -> c) -> (a -> b -> c)
-- tiene que recibir los parametros de forma no currificada 
-- curry f :: (a, b) -> c -> d
-- curry curry f:: (((a,b), c)-> d )-> a -> b -> c -> d
-- curry f x y = curry (curry f (x, y)) => \x -> \y -> curry (curry f (x, y)) => \x -> \y -> curry (a -> b -> c)
-- esto quiere decir que si tenemos curry curry inicialmente teniamos una tupla con otra dentro ((a,b), c)
-- entonces el segundo curry recibiria ((a, b) -> c) -> d y devolveria a -> b -> c -> d

-- curry:: ((a, b )-> c) -> (a -> b -> c)
-- curry f x y = f (x,y)
-- curry f x = \ y  -> f (x, y)
-- funcion que toma un x y devuelve una funcion del tipo y -> f (x,y)
-- curry f = \x -> \y -> f (x,y)     f (x,y) = c


-- 21 i) Explique a grandes rasgos las principales diferencias entre recursión de pila y de cola.
-- En la recursión de pila, la llamada recursiva NO es lo último que ocurre en la función. Cada llamada queda pendiente de una operación, por lo que se acumulan frames en la pila de ejecución. Si la lista es muy larga, puede dar stack overflow.
-- Ejemplo:
-- sumarLista :: [Int] -> Int
-- sumarLista [] = 0
-- sumarLista (x:xs) = x + sumarLista xs

-- En la recursión de cola, la llamada recursiva es la última operación que se hace en la función. Esto permite que el compilador pueda hacer una optimización de tail-call, y no acumula pila.
-- Ejemplo:
-- sumarListaTR :: [Int] -> Int
-- sumarListaTR xs = sumarAux xs 0
 
-- sumarAux [] acc = acc
-- sumarAux (x:xs) acc = sumarAux xs (acc + x)

-- Cada llamada a sumarAux solo pasa nuevos parámetros, sin operaciones pendientes.
 -- ii) Enumere funciones anteriores que puedan definirse de una forma más sencilla con recursión de pila que de cola, y viceversa.


 -- 22 Definir las funciones factorial, suma (todos los elementos de una lista), rev, append y aparear de manera que sean recursivas de cola.

factorial:: Int -> Int
factorial n = factAux n 1 
  where 
    factAux 0 acc = acc 
    factAux n acc = factAux (n - 1) (n * acc)
    -- es lo mismo que crear otra funcion nada mas que esta solo se puede usar dentro de factorial

-- opcion con let
-- factorial :: Int -> Int
-- factorial n =
--   let factAux 0 acc = acc
--       factAux n acc = factAux (n - 1) (n * acc)
--   in factAux n 1

-- factorial n = let factAux ... in factAux n 1 internamente hace esto

sumaListaB:: [Int] -> Int
sumaListaB xs = sumAux xs 0 
  where 
    sumAux [] n = n
    sumAux (x:xs) n = sumAux xs (n + x) 

rev:: [a] -> [a]
rev xs = revAux xs []
  where
    revAux [] ys = ys
    revAux (x:xs) ys = revAux xs (x:ys)

append :: [Int] -> [Int] -> [Int]
append xs ys = appendAux xs ys xs
  where
    appendAux [] _ acum = acum
    appendAux as (b:bs) acum = appendAux as bs (b:(acum))

-- no se puede hacer append de cola sin modificar el orden
-- append :: [a] -> [a] -> [a]
-- append xs ys = appendAux (rev xs) ys
--   where
--     appendAux [] acc = acc
--     appendAux (x:xs) acc = appendAux xs (x : acc)

-- [1,2,3] [5,6,7]
-- [3,2,1] [5,6,7]
-- [2,1] [3,5,6,7]
-- [1] [2,3,5,6,7]

aparear :: [a] -> [b] -> [(a,b)]
aparear xs ys = aparearAux xs ys []
  where
    aparearAux [] _ acc = reverse acc
    aparearAux _ [] acc = reverse acc
    aparearAux (x:xs) (y:ys) acc = aparearAux xs ys ((x,y) : acc)

-- 23 Definir las funciones par (devuelve si un número es par o no) e impar usando recursión mutua (indirecta).
-- ii) Definir las funciones cong0, cong1 y cong2 (devuelve si un número es congruente a cero, uno o dos respectivamente) usando recursión mutua.
par:: Int -> Bool
par 0 = True
par n = impar (n - 1)

impar:: Int -> Bool
impar 0 = False
impar n = par (n - 1)

-- ii) Definir las funciones cong0, cong1 y cong2 (devuelve si un número es congruente a cero, uno o dos respectivamente) usando recursión mutua.
-- no me acuerdo que era esto

-- 24 falta hacer
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f b [] = b
-- foldr f b (x:xs) = f x (foldr f b xs)

--   foldl :: (b -> a -> b) -> b -> [a] -> b
--   foldl f b [] = b
--   foldl f b (x:xs) = foldl f (f b x) xs


-- 25 
sumaListaFR:: [Int] -> Int
sumaListaFR = foldR (+) 0
-- foldr (\x acc -> x + acc) 0

idFR :: [a] -> [a]
idFR = foldr (:) []
-- idFR xs = foldR (\ x accum -> (x:accum) ) [] xs

memberFR::Int -> [Int] -> Bool
memberFR x xs = foldR (\ n acc -> n == x || acc) False xs

appendFR:: [a] -> [a] -> [a]
appendFR xs ys = foldR (:) ys xs

revFR:: [a] -> [a]
revFR xs = foldR (\ x acc -> acc ++ [x]) [] xs

filterFR:: (a -> Bool) -> [a] -> [a]
filterFR f xs = foldR (\ x acc -> if (f x) then (x:acc) else acc) [] xs

mapFR:: (a -> b) -> [a] -> [b]
mapFR f xs = foldR (\x acc -> f x:acc) [] xs

-- Norma 2 de un vector (raíz cuadrada de suma de cuadrados)
norma2 :: Floating a => [a] -> a
norma2 = sqrt . foldr (\x acc -> x^2 + acc) 0

-- Aplanar una lista de listas
flat :: [[a]] -> [a]
flat = foldr (++) []

-- Inserta un elemento ordenadamente en una lista ordenada
insertar :: Ord a => a -> [a] -> [a]
insertar x = foldr (\y acc -> if x <= y then x : y : acc else y : acc) [x]

-- Ordenamiento por inserción
insort :: Ord a => [a] -> [a]
insort = foldr insertar []

-- Partes (potencia del conjunto)
partes :: [a] -> [[a]]
partes = foldr (\x acc -> acc ++ map (x:) acc) [[]]

compFuncs :: [a -> a] -> (a -> a)
compFuncs = foldr (.) id
-- siempre hay acumulador!!!

-- ii los mismos pero con foldl
sumaListaFL:: [Int] -> Int
sumaListaFL = foldL (+) 0

idFL :: [a] -> [a]
idFL xs = rev (foldL (\ acc x -> (x:acc)) [] xs)
-- idFL xs = foldL (flip (:)) [] xs
-- idFL xs = foldl (++) [] (map (:[]) xs)

memberFL::Int -> [Int] -> Bool
memberFL x xs = foldL (\ acc n -> n == x || acc) False xs

appendFL:: [a] -> [a] -> [a]
appendFL xs ys = foldR (:) ys xs

revFL::[a] -> [a]
revFL xs = foldL (\ acc x -> (x:acc) ) [] xs
-- revL = foldl (flip (:)) []

--  partes (conjunto potencia): Necesita aplicar map (x:) acc, y ese acc se usa tanto directa como internamente en map. foldl no es adecuado aquí porque la acumulación izquierda no permite reutilizar acc en el mismo paso.

-- map y filter pueden definirse con foldl, pero no son eficientes (porque usan ++ o reverse)
filterFL:: (a -> Bool) -> [a] -> [a]
filterFL f xs = foldL (\ acc x -> if (f x) then acc ++ [x] else acc) [] xs

mapFL:: (a -> b) -> [a] -> [b]
mapFL f xs = foldL (\ acc x -> f x:acc) [] xs

-- mapL f = reverse . foldl (\acc x -> f x : acc) []
-- filterL p = reverse . foldl (\acc x -> if p x then x : acc else acc) []

--25iii Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista 
--(da como resultado el primer elemento menos el segundo más el tercero menos el cuarto y así) usando foldr.  
-- ¿Por qué no se puede hacer usando foldl?
sumaAlt:: [Int] -> Int
sumaAlt xs = foldR (\ n acc ->  - n - acc ) 0 xs


-- 26i Definir la función esPrimo, que dado un número dice si es primo o no. Usar map, foldr y «. Considerar (si hace falta) que 1 es primo.
(<->):: Int -> Int -> [Int]
x <-> n = if x /= n then x:((x+1) <-> n) else [x]

-- (<->):: Int -> Int -> [Int]
-- a <-> b = [a..b]

esPrimo:: Int -> Bool
esPrimo 1 = True
esPrimo n = not (foldr (||) False (map (\ x -> mod n x == 0) ((<->) 2 (n - 1))))


-- 26ii Calcular la cantidad de primos mellizos menores a 1000 con el reductor cantMell. Dos números se dicen primos mellizos si ambos 
-- son primos y su diferencia es 2. Usar , map, esPrimo y foldr.
-- cantMell1000::Int

cantMell:: Int
cantMell = foldR (\ x acc -> if (esPrimo (x + 2)) then acc + 1 else acc ) 0 (filter esPrimo (1 <-> 1000))

-- con map sin filter
-- cantMell = foldR (\ x acc -> if (esPrimo (x + 2)) then acc + 1 else acc ) 0 (filter (\ y -> y /= 0) (map (\x -> if esPrimo x then x else 0) (1 <-> 1000)))
-- otra opcion
-- esPrimo :: Int -> Bool
-- esPrimo n
--   | n <= 1    = False
--   | otherwise = not (tieneDivisorDesde 2)
--   where
--     tieneDivisorDesde i
--       | i * i > n      = False
--       | mod n i == 0   = True
--       | otherwise      = tieneDivisorDesde (i + 1)

-- cantMell :: Int
-- cantMell = foldr (+) 0 (map (\(x, y) -> if esPrimo x && esPrimo y then 1 else 0) [(x, x+2) | x <- [1..997]])


-- 27 Dada la siguiente función (variante de foldr):

foldrb :: (c -> b -> b) -> (a -> c) -> b -> [a] -> b
foldrb f g b [] = b
foldrb f g b (x:xs) = f (g x) (foldrb f g b xs)

-- Usarla para definir sumaLista, long y map
sumaListaFRb:: [Int] -> Int
sumaListaFRb xs = foldrb (+) id 0 xs

longFRB:: [ a ] -> Int
longFRB xs = foldrb (\ x acc -> acc + 1 ) id 0 xs

mapFRb:: (a -> b ) -> [a] -> [b]
mapFRb f xs = foldrb (:) f [] xs

-- 28.	i)	Definir una variante de foldr para usarla en la definición de la función maxl.
foldrMaxl::(a -> a -> a) -> [a] -> a
foldrMaxl f (x:[]) = x
foldrMaxl f (x:xs) = f x (foldrMaxl f xs)

maxl :: Ord a => [a] -> a --suponemos que es una lista de enteros
maxl = foldrMaxl max

-- la funcion que le vamos a pasar a foldrMaxl es max, va a terminar buscando el max entre todos los elementos de la lista, cuando desapila
-- foldr1' max [3,5,1]
-- → max 3 (foldr1' max [5,1])
-- → max 3 (max 5 (foldr1' max [1]))
-- → max 3 (max 5 1)
-- → max 3 5
-- → 5

-- ii) Definir una variante de foldr para usarla sobre el tipo ArbBin (árboles binarios rotulados), y otra versión sobre el tipo ArbBinRotHoj (
-- árboles binarios con rótulos sólo en las hojas).
data ArbBinB a b = HojaB a | NodoB a (ArbBinB a b) (ArbBinB a b) deriving Show

foldRArb :: (a -> b) -> (a -> b -> b -> b) -> ArbBinB a b -> b -- la primera es una funcion para procesar las hojas.
foldRArb h n (HojaB x) = h x
foldRArb h n (NodoB x i d) = n x (foldRArb h n i) (foldRArb h n d)

data ArbBinRotHoj a = HojaRot a | NodoRot (ArbBinRotHoj a ) (ArbBinRotHoj a ) deriving Show

foldRArbRot :: (a -> b) -> (b -> b -> b) -> ArbBinRotHoj a -> b -- la primera es una funcion para procesar las hojas.
foldRArbRot h n (HojaRot x) = h x
foldRArbRot h n (NodoRot i d) = n (foldRArbRot h n i) (foldRArbRot h n d)

-- iii) Definir una variante de foldr para usarla en la definición de la función duplicados, que tome una lista y devuelva otra con los elementos que aparecían 
-- más de una vez en la lista original. Ej: duplicados [1,2,3,2,1] ® [1,2].

foldrDuplicados :: Eq a => a -> ([a], [a]) -> ([a], [a])
foldrDuplicados x (dup, acc) 
  | x `elem` acc && not (x `elem` dup) = (x : dup, acc)
  | otherwise = (dup, x : acc)

duplicados :: Eq a => [a] -> [a]
duplicados xs = reverse $ fst $ foldr foldrDuplicados ([], []) xs

member :: Eq a => [a] -> a -> Bool
member [] a = False
member (x:xs) a = if x == a then True else (member xs a) 

-- foldrVariante:: (a -> [a] -> [a]) -> [a] -> [a]
-- foldrVariante _ [] = []
-- foldrVariante f (x:xs) = f x (foldrVariante f xs)

foldrWithState :: (a -> (b, c) -> (b, c)) -> (b, c) -> [a] -> (b, c)
foldrWithState _ acc []     = acc
foldrWithState f acc (x:xs) = f x (foldrWithState f acc xs)

-- duplicados:: Eq a => [a] -> [a]
-- duplicados xs = foldrVariante (\ x acc -> if ((member acc x)) then acc else if member xs x then (x:acc) else acc) xs -- esto no anda

-- 29 i) Programar una función cant currificada usando foldr por un lado y foldl por otro, que dada una función que refleja una condición y una lista, devuelva la 
--cantidad de elementos que cumplen la condición. ¿Por qué‚ las dos versiones (usando foldr y foldl) son tan similares?
