import Data.Char (intToDigit)


-- 1i dado un entero devuelve 1, 0 ó -1 dependiendo si el entero dado es positivo, cero o negativo respectivamente. 
signo :: Int -> Int
signo 0 = 0
signo n = if n > 0 then 1 else -1

-- signo n
--   | n > 0     = 1
--   | n < 0     = -1
--   | otherwise = 0

-- 1ii dado un entero devuelve si es negativo o no. Usar signo.
negativo :: Int -> Bool
negativo n = signo n == -1

-- 2i dados dos números devuelve el máximo de los dos. En Haskell esta función está predefinida para los tipos ordenados y se llama también max.
maxi :: Int -> Int -> Int
maxi a b = if a >= b then a else b

-- maxi a b
--   | a >= b    = a
--   | otherwise = b

-- 2ii que dados tres números devuelve el máximo de los tres. Usar max.
max3 :: Int -> Int -> Int -> Int
max3 a b c = maxi (maxi a b) c

-- 2iii dados dos números devuelve el mínimo de los dos. En Haskell esta función está predefinida para los tipos ordenados y se llama también min. Usar max.
mini :: Int -> Int -> Int
mini a b = if maxi a b == a then b else a

-- mini a b
--   | maxi a b == a = b
--   | otherwise     = a

-- 3i factorial
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

--3ii compbinatorio
combinatorio :: Int -> Int -> Int
combinatorio n k = fac n `div` (fac k * (fac (n - k)))

--3iii
fibonacci :: Int -> Int
fibonacci 1 = 0 --considere que arrancaba en 1, sino seria fibonacci de 0
fibonacci 2 = 1 
fibonacci n = fibonacci(n-1) + fibonacci (n-2)  

-- 3iv divisiblePor, que devuelve si un número es divisible por otro.
divisiblePor :: Int -> Int -> Bool
divisiblePor x 0 = False
divisiblePor x y = mod x y  == 0

--4i esVacia que devuelve si una lista está vacía o no. En Haskell esta función se llama null.
esVacia :: [a] -> Bool
esVacia [] = True
esVacia [a] = False
-- ["holaa":["sda"]]

-- 4ii Definir las funciones cabeza y resto, que devuelven el primer elemento y del segundo al último elemento de una lista, respectivamente. ¿Pueden realizarse estas funciones sin usar apareamiento de patrones (pattern matching)? En Haskell estas funciones se llaman head y tail respectivamente.
cabeza :: [a] -> a
cabeza (a:as) = a

resto :: [a] -> [a]
resto (a:as) = as

-- 5i
long :: [a] -> Int
long [] = 0
long (a:as) = 1 + long (as)

-- 5ii
sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (a:xs) = a + sumaLista (xs) 

-- 5iii
member :: [Int] -> Int -> Bool
member [] a = False
member (x:xs) a = x == a || member xs a 

-- 5iv
append :: [Int] -> [Int] -> [Int]
append [] [] = []
append (a:as) [] = (a:as)
append [] (b:bs) = (b:bs)
append (a:as) (b:bs) = [a] ++ (append as (b:bs))

tomar :: [a] -> Int -> [a]
tomar [] n = []
tomar (a:as) 0 = []
tomar (a:as) n = [a] ++ tomar as (n-1)

term :: [a] -> Int -> a
-- term [] n = "error"
term (a:as) 0 = a -- pongo 1 porque considero que esa es la primera posicion, sino seria 0
term (a:as) n = term as (n-1)

rev :: [a] -> [a]
rev [] = []
rev (a:as) = rev as ++ [a]

maxl :: [Int] -> Int --suponemos que es una lista de enteros
maxl (a:[]) = a
maxl (a:as) = if a> maxl as then a else maxl as

-- maxl' :: [Int] -> Int -- alternativa con foldl
-- maxl' [] = error "Lista vacia"
-- maxl' (a:as) = foldl maxi a as


cuenta :: [Int] -> Int -> Int -- tuve que poner int, porque para a deberia estar definida la operacion ==
cuenta [] n = 0
cuenta (a:as) n = if a == n then cuenta as n + 1 else cuenta as n

repite :: Int -> a -> [a] -- se llama replicate
repite 0 x = []
repite n x = [x] ++ repite (n-1) x

consec :: Int -> Int -> [Int]
p `consec` u = if p == u then [p] else [p] ++ ((p+1) `consec` u )

-- 6ii, medio raro pero solo encontre esta forma de definir factorial usando consec (salvo que se pueda hacer un for)
mult :: [Int] -> Int
mult [] = 1
mult (a:as) = a * mult as

fac2 :: Int -> Int
fac2 n = mult (1 `consec` n)

-- 7i con recursion explicita
ult :: [a]-> a
ult (a:[]) = a
ult (a:as) = ult as

-- 7i con recursion implicita
ulti :: [a]-> a
ulti (a:as) = head(rev (a:as))

-- 7ii con recursion explicita
sacarUltimo :: [a] -> [a]
sacarUltimo (a:[]) = []
sacarUltimo (a:as) = [a] ++ sacarUltimo as

-- 7ii con recursion implicita
sacarUltimoI :: [a] -> [a]
sacarUltimoI (a:[]) = []
sacarUltimoI (a:as) = [a] ++ rev(resto(rev as))
-- otra forma sacarUltimoI (a:as) = rev(resto(rev (a:as)))


-- 8
capicua :: Eq a => [a] -> Bool -- lo hice con int, no puedo poner a porque no funcionaria para todos los tipos
capicua (a:[]) = True
capicua [] = True
capicua (a:as) = if (a == (last as)) then (capicua (sacarUltimo(as))) else False

-- 9i flat
flat :: [[a]] -> [a]
flat [[]] = []
flat ((a:as):[[]]) = (a:as)
flat ([]:(b:bs)) = flat (b:bs)
flat (a:as) = [head(a)] ++ (flat((resto a):as))

-- 9ii
longLi :: [[a]] -> Int
longLi [[]] = 0
longLi (a:as) = long(flat(a:as))

-- 10i intercalar
intercalar :: [a] -> [a] -> [a]
intercalar [][] = []
intercalar [](a:as) = (a:as)
intercalar (a:as)[] = (a:as)
intercalar (a:as) (b:bs) = [a] ++ [b] ++ (intercalar as bs)

-- 10ii
aparear :: [Int] -> [Int] -> [Int]
aparear [] [] = []
aparear [] (b:bs) = (b:bs)
aparear (a:as)[] = (a:as)
aparear (a:as)(b:bs) = if a < b then ([a] ++ (aparear as (b:bs))) else ([b] ++ (aparear (a:as) bs))

-- 11 decAHex
digHex :: Int -> [Char]
digHex 0 = ['0']
digHex n 
       | n <= 9 = [intToDigit n] 
       | n == 10 = ['A']
       | n == 11 = ['B']
       | n == 12 = ['C']
       | n == 13 = ['D']
       | n == 14 = ['E']
       | otherwise = ['F']

decAHex :: Int -> [Char]
decAHex 0 = []
decAHex n =  decAHex (n `div` 16) ++ (digHex (n `mod` 16))

-- 12 numero perfecto
divisores :: Int -> [Int]
divisores 0 = []
divisores 1 = [1]
divisores n = filter f [1..n-1]
  where
    f m = mod n m == 0

divisors ::  Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]
-- x <- [1..n]: Itera sobre los números del 1 al n
-- filter recibe una función de filtro, pred, y una lista, xs, y devuelve una lista con los elementos de xs que haya pasado el filtro.

perfecto:: Int -> Bool
perfecto 0 = True
perfecto n = sumaLista(divisores n) == n

-- 13i
esPrefija::[Int] -> [Int] -> Bool
esPrefija x [] = False
esPrefija [] y = True
esPrefija (x:xs) (y:ys) = x == y && (esPrefija xs ys)

-- 13ii mi version
posicion ::Eq a => ([a], [a]) -> Int
posicion ([],y) = 1
posicion (x,[]) = 0
posicion ((x:xs), (y:ys)) = if x == y then (posicion (xs, ys)) else 1 + (posicion ((x:xs), ys))

-- otra
primerosNTerminos :: Int -> [a] -> [a] 
primerosNTerminos 0 xs = []
primerosNTerminos n [] = []
primerosNTerminos n (x:xs) = x:(primerosNTerminos (n-1) xs)

posi ::Eq a => ([a], [a]) -> Int
posi (x,[]) = 0
posi ((x:xs), (y:ys)) = if (x:xs) /= primerosNTerminos (long (x:xs)) (y:ys) then 1 + posi ((x:xs), ys) else 1 

-- 13 iii
subcadena:: [Char] -> Int -> Int -> [Char]
subcadena (x:xs) 1 1 = [x]
subcadena (x:xs) p l = if p == 1 then [x] ++ (subcadena xs p (l-1)) else subcadena xs (p-1) l

-- 14 no me salio
-- quicksort::[Int] -> [Int]
-- quicksort [] = []
-- quicksort (a:as) = a quicksort (subcadena (a:as) 1 long(as) /2 )

-- 15 partes
agregar :: a -> [[a]] -> [[a]]
agregar x [] = []
agregar x (ys:yss) = (x:ys): agregar x yss

partes:: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = partes xs ++ agregar x (partes xs)

-- 15 ii
-- solucion en la clase, recursion de pila. Hay que meter la cabeza en todas las posiciones
permutaciones:: [a] -> [[a]]
permutaciones [] = [[]] -- ([]:[])
-- permutacines (a:[]) = [[a]]
permutaciones (x:xs) = insertarElem x (permutaciones xs)

insertarElem :: a -> [[a]] -> [[a]]
insertarElem x [] = []
insertarElem x (a:as) = (insertar x a) ++ (insertarElem x as) -- aca se puede usar ++ porque son del mismo tipo


insertar:: a -> [a] ->[[a]] -- la cantidad de posiciones en las que puede ir va desde 0 hasta la longitud de la lista
insertar x [] = [[x]]
insertar x xs = insertarPosLista [0..(length xs)] x xs

insertarPosLista:: [Int] -> a -> [a] -> [[a]]
insertarPosLista [] x xs = []
insertarPosLista (p:ps) x xs = ((insertarPos p x xs)):((insertarPosLista ps x xs))

insertarPos:: Int -> a -> [a] -> [a]
insertarPos n x xs = (take n xs) ++ x:(drop n xs) -- arrancamos en 0

-- take, toma los n primeros elementos de la lista
-- drop, elimina los n primeros elementos de la lista

-- solucion de Fran
-- idea: agregar la cabeza a todas las permutaciones de la reversa resto
insertaEnTodasPos :: a -> [a] -> [[a]]
insertaEnTodasPos x [] = [[x]]
insertaEnTodasPos x (y:ys) = (x:y:ys) : map (y:) (insertaEnTodasPos x ys)

añadir :: a -> [[a]] -> [[a]]
añadir x xss = concatMap (insertaEnTodasPos x) xss

perms :: [a] -> [[a]]
perms [] = []
perms [x] = [[x]]
perms (x:xs) = añadir x (perms xs)

{-
comentario multilinea
-}