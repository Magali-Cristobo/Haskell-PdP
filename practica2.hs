-- Definir la función hanoi, que dada una cierta cantidad de discos (mayor a cero), devuelva la lista de movimientos (de poste a poste) que deberían realizarse para resolver el juego de las torres de Hanoi. Especificar cómo son los tipos Poste y Movimiento.
-- type Poste = [Int] -- es mas una pila que otra cosa no era asi al final
type Movimiento = (Poste, Poste) -- (posteInicial, posteFinal) siempre se desapila el primero entonces saque el parametro de disco. Estaba bien!!
data Poste = Origen | Destino | Auxiliar deriving (Eq, Show)

hanoi :: Int -> [Movimiento] -- se puede poner el tipo movimientos para evitar la lista
hanoi n = hanoi2 n Origen Auxiliar Destino 
-- origen -> auxiliar      auxiliar -> destino

hanoi2 :: Int -> Poste -> Poste -> Poste -> [Movimiento]
hanoi2 0 po pd pa = []
hanoi2 n po pd pa = (hanoi2 (n-1) po pa pd) ++ ((po, pd):(hanoi2 (n-1) pa pd po)) -- cambiamos los roles de cada poste

-- 2 Definir el tipo Extension, que “agrega” un elemento indefinido a otro tipo dado
data Extension a = Nada | Solo a deriving Show

cabeza :: [a] -> Extension a
cabeza [] = Nada
cabeza (a:as) = Solo a -- es asi??

-- 3 Definir el tipo TipoDeSangre

data TipoDeSangre = Tipo String Char deriving Show
puedeDonarA :: TipoDeSangre -> TipoDeSangre -> Bool
puedeDonarA (Tipo "0" 'n') (Tipo _ _) = True
puedeDonarA (Tipo a 'n') (Tipo b d) = a==b
puedeDonarA (Tipo a 'p') (Tipo b c) = c == 'p' && (a == b || b == "AB") 

-- 4
data Nat = Cero | Suc Nat deriving (Eq, Show)

esCero :: Nat -> Bool
esCero Cero = True
esCero (Suc _) = False

predecesor :: Nat -> Nat
predecesor (Suc n) = n

suma :: Nat -> Nat -> Nat
suma Cero m = m
suma (Suc n) m = Suc (suma n m) 

suma2 :: Nat -> Nat -> Nat
suma2 n m = if (esCero n) then m else Suc (suma2 (predecesor n) m)

resta:: Nat -> Nat -> Nat
resta n Cero = n
-- resta n m = if (esCero m) then n else Suc (resta (sucesor? n) m)

producto :: Nat -> Nat -> Nat
producto n (Suc m) = if n == Cero || m == Cero then Cero else suma n (producto n m )

menorIgual :: Nat -> Nat -> Bool
menorIgual n Cero = n == Cero
menorIgual Cero n = True
menorIgual (Suc n) (Suc m) = menorIgual n m 

-- 5 tiene que ver con el calculo lambda, aun no lo vimos

-- data LambdaTerm = Crear [Lambda]

-- 6 Definir el tipo ListOrd (lista ordenada). La diferencia con las listas comunes es que la inserción de un dato a la lista lo hace en forma ordenada. Crear funciones similares a la del tipo lista y una función de conversión a lista común.
data ListaOrd a = LO [a] deriving (Eq, Ord, Show)

insertar :: Ord a => a  -> ListaOrd a -> ListaOrd a
insertar n (LO []) = LO [n]
-- insertar n (LO (a:as)) = if n >= a then (a:(LO(insertar n (LO as)))) else LO (n:(a:as)) -- no me dejaba poner esto
insertar n (LO (a:as)) = if n >= a then LO (a:az)  else LO (n:(a:as)) where LO az = insertar n (LO as)

-- otra forma
insert :: Ord a => a -> ListaOrd a -> ListaOrd a
insert x (LO xs) = LO (insert' x xs) where insert' x [] = [x]
                                           insert' x (y:ys) = if x<=y then (x:(y:ys)) else (y:insert' x ys)

{- 7
si definimos un tipo con participacion de otro, estamos limitando el uso justamente para ese tipo, si lo hacemos sin ningun tipo existente, tenemos mas posibilidades
de que se aplique a cualquier tipo existente, pero es probable que se requiera que ese tipo sea de algunas clases como show, eq, ord, etc
Creo que no preguntaba esto
-}

-- 8 Definir el tipo Pila con funciones crear, esVacia, cabeza, agregar, sacar y cantidad. Idem con el tipo Cola.
data Pila a = PL [a]  deriving (Show)

crear:: a -> Pila a
crear x = PL [x]

esVacia:: Pila a -> Bool
esVacia (PL [])  = True
esVacia (PL xs) = False

cabezaP:: Pila a -> a
cabezaP (PL (a:as)) = a 

agregarP:: Pila a -> a -> Pila a
agregarP (PL []) x = PL [x]
agregarP (PL xs) x =  PL (x:xs) 

sacarP:: Pila a -> Pila a
sacarP (PL []) = PL []
sacarP (PL (a:as)) = PL as

cantidadP :: Pila a -> Int
cantidadP (PL []) = 0 
cantidadP (PL (a:as)) = 1 + (cantidadP (PL as))

data Cola a = CL [a]  deriving (Show)

crearC:: a -> Cola a
crearC x = CL [x]

esVaciaC:: Cola a -> Bool
esVaciaC (CL [])  = True
esVaciaC (CL xs) = False

cabezaC:: Cola a -> a
cabezaC (CL (a:[])) = a
cabezaC (CL (a:as)) = cabezaC (CL as)

agregarC:: Cola a -> a -> Cola a
agregarC (CL []) x = CL [x]
agregarC (CL xs) x =  CL (x:xs) 

sacarC:: Cola a -> Cola a
sacarC (CL (a:[])) = CL []
sacarC (CL (a:as)) = CL(a:az) where CL az = sacarC (CL as)

cantidadC :: Cola a -> Int
cantidadC (CL []) = 0 -- esto no deberia estar
cantidadC (CL (a:as)) = 1 + (cantidadC (CL as))

-- 9 i Definir el tipo ArbBin (árbol binario rotulado) con sus funciones constructoras y selectoras.
data ArbBin a = Hoja a | Nodo a (ArbBin a) (ArbBin a) deriving Show

crearArbol :: a -> ArbBin a -> ArbBin a -> ArbBin a
crearArbol a si sd = Nodo a si sd

-- crearArbolVacio?
nroNodos :: ArbBin a -> Int
nroNodos (Hoja n) = 0
nroNodos (Nodo a si sd) = 1+ (nroNodos si) + (nroNodos sd)

nroHojas :: ArbBin a -> Int
nroHojas (Hoja n) = 1
nroHojas (Nodo a si sd) = (nroHojas si) + (nroHojas sd)

altura:: ArbBin a -> Int
altura (Hoja n) = 1
altura (Nodo a si sd) = 1 +  maximo (altura si) (altura sd)

maximo:: Int -> Int -> Int
maximo n m = if n > m then n else m

preorden:: ArbBin a -> [a]
preorden (Hoja r) = [r]
preorden (Nodo r si sd) = [r] ++ (preorden si) ++ (preorden sd)

inorden:: ArbBin a -> [a]
inorden (Hoja r) = [r]
inorden (Nodo r si sd) =  (inorden si) ++ [r] ++ (inorden sd)

posOrden:: ArbBin a -> [a]
posOrden (Hoja r) = [r]
posOrden (Nodo r si sd) =  (posOrden si) ++ (posOrden sd) ++ [r] 

igEstrucArb :: ArbBin a -> ArbBin a -> Bool
igEstrucArb (Hoja a) (Hoja b) = True
igEstrucArb (Nodo a si sd) (Hoja b) = False
igEstrucArb  (Hoja b) (Nodo a si sd)  = False
igEstrucArb  (Nodo a si sd)(Nodo b sii sdd) = (igEstrucArb si sii) && (igEstrucArb sd sdd)

-- 10
data ArbBinRotHoj a = HojaRot a | NodoSinRot (ArbBin a) (ArbBin a) deriving Show

-- 11
data ArbGen a = HojaGen a | NodoGen a [ArbGen a]  deriving Show

crearArbolGen :: a -> [ArbGen a] -> ArbGen a
crearArbolGen a ds = (NodoGen a ds)

-- crearArbolVacio?
nroNodosGen :: ArbGen a -> Int
nroNodosGen (HojaGen n) = 0
nroNodosGen (NodoGen a (x:xs)) = 1 + (nroNodosGen x) + (nroNodosGen2 xs)

nroNodosGen2 :: [ArbGen a] -> Int
nroNodosGen2 [] = 0
nroNodosGen2 (a:as) = nroNodosGen (a) + nroNodosGen2 (as)


nroHojasGen :: ArbGen a -> Int
nroHojasGen (HojaGen n) = 1
nroHojasGen (NodoGen n (a:as)) = (nroHojasGen a) + (nroHojasGen2 as)

nroHojasGen2 :: [ArbGen a] -> Int
nroHojasGen2 [] = 0
nroHojasGen2 (a:as) = (nroHojasGen a) + (nroHojasGen2 as)

maxl :: [Int] -> Int --suponemos que es una lista de enteros
maxl (a:[]) = a
maxl (a:as) = if a> maxl as then a else maxl as

alturaGen:: ArbGen a -> Int
alturaGen (HojaGen n) = 1
alturaGen (NodoGen n (a:as)) = 1 +  maxl ((alturaGen a) :(alturaGen2 as))

alturaGen2:: [ArbGen a] -> [Int]
alturaGen2 [] = []
alturaGen2 (a:as) =  ((alturaGen a) : (alturaGen2 as))

preordenGen:: ArbGen a -> [a]
preordenGen (HojaGen r) = [r]
preordenGen (NodoGen r (a:as)) = [r] ++ (preordenGen a) ++ (preordenGen2 as)

preordenGen2:: [ArbGen a] -> [a]
preordenGen2 [] = []
preordenGen2 ((a:as)) = (preordenGen a) ++ (preordenGen2 as)

inordenGen:: ArbGen a -> [a]
inordenGen (HojaGen r) = [r]
inordenGen (NodoGen r (a:as)) =  (inordenGen a) ++ [r] ++ (inordenGen2 as)

inordenGen2:: [ArbGen a] -> [a]
inordenGen2 [] = []
inordenGen2 ((a:as)) = (inordenGen a) ++ (inordenGen2 as)

posordenGen:: ArbGen a -> [a]
posordenGen (HojaGen r) = [r]
posordenGen (NodoGen r (a:as)) =  (posordenGen a) ++  (posordenGen2 as) ++ [r]

posordenGen2:: [ArbGen a] -> [a]
posordenGen2 [] = []
posordenGen2 ((a:as)) = (posordenGen a) ++ (posordenGen2 as)

long :: [a] -> Int
long [] = 0
long (a:as) = 1 + long (as)

igEstrucArbGen :: ArbGen a -> ArbGen a -> Bool
igEstrucArbGen (HojaGen a) (HojaGen b) = True
igEstrucArbGen (NodoGen a (x:xs)) (HojaGen b) = False
igEstrucArbGen  (HojaGen b) (NodoGen a (x:xs))  = False
igEstrucArbGen  (NodoGen a (x:xs)) (NodoGen b (y:ys)) = (long xs) == (long ys) && (igEstrucArbGen2 xs ys)

igEstrucArbGen2 :: [ArbGen a] -> [ArbGen a] -> Bool
igEstrucArbGen2 [] [] = True
igEstrucArbGen2 [] xs = False
igEstrucArbGen2 xs [] = False
igEstrucArbGen2 (x:xs) (y:ys) = (igEstrucArbGen x y) && (igEstrucArbGen2 xs ys)

-- 12 
data Conjunto a = C [a] deriving (Show, Eq, Ord)

esVacio:: Conjunto a -> Bool
esVacio (C[]) = True
esVacio (C x) = False 

union:: Eq a => Conjunto a -> Conjunto a -> Conjunto a
union (C[]) (C y) = (C y) 
union (C x) (C []) = (C x) 
union (C x) (C y) = (C (appendC x y))

member :: Eq a => [a] -> a -> Bool
member [] a = False
member (x:xs) a = if x == a then True else (member xs a) 

appendC:: Eq a => [a] -> [a] -> [a]
appendC [] y = y
appendC x  [] = x
appendC (x:xs) (y:ys) = if (member (y:ys) x) then (appendC xs (y:ys)) else ([x] ++ appendC xs (y:ys))

agElem:: Eq a => Conjunto a -> a -> Conjunto a
agElem (C [] ) x = (C [x])
agElem (C xs) x = if not(member xs x) then ((C ([x]++ xs))) else (C xs)

interseccion:: Eq a => Conjunto a -> Conjunto a -> Conjunto a
interseccion (C []) _ = (C [])
interseccion _ (C []) = (C [])
interseccion (C (x:xs)) (C (y:ys)) = if ((member ys x) || x == y) then (agElem (interseccion (C xs) (C (y:ys)) ) x ) else (interseccion (C xs) (C (y:ys)))

sacElem:: Eq a => Conjunto a -> a -> Conjunto a
sacElem (C []) x = (C [])
sacElem (C (a:as)) x = if x == a then (C as) else (agElem(sacElem (C as) x) a)

-- 13. Definir el tipo Matriz de números con operaciones de suma, trasposición y producto.

