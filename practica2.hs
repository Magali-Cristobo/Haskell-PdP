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

data ArbBinH a b = Hoja a | Nodo b (ArbBinH a b) (ArbBinH a b) deriving Show

crearArbol :: b -> ArbBinH a b -> ArbBinH a b -> ArbBinH a b
crearArbol a si sd = Nodo a si sd

-- Preorden: Nodo, Izquierda, Derecha
preordenH :: ArbBinH a b -> [Either a b]
preordenH (Hoja h)        = [Left h]
preordenH (Nodo r i d) = [Right r] ++ preordenH i ++ preordenH d

-- Inorden: Izquierda, Nodo, Derecha
inordenH :: ArbBinH a b -> [Either a b]
inordenH (Hoja h)        = [Left h]
inordenH (Nodo r i d) = inordenH i ++ [Right r] ++ inordenH d

-- Postorden: Izquierda, Derecha, Nodo
postordenH :: ArbBinH a b -> [Either a b]
postordenH (Hoja h)        = [Left h]
postordenH (Nodo r i d) = postordenH i ++ postordenH d ++ [Right r]

nroNodos :: ArbBinH a b -> Int
nroNodos (Hoja n) = 0
nroNodos (Nodo a si sd) = 1+ (nroNodos si) + (nroNodos sd)

nroHojas :: ArbBinH a b -> Int
nroHojas (Hoja n) = 1
nroHojas (Nodo a si sd) = (nroHojas si) + (nroHojas sd)

altura:: ArbBinH a b -> Int
altura (Hoja n) = 1
altura (Nodo a si sd) = 1 +  maximo (altura si) (altura sd)

igEstrucArb :: ArbBinH a b -> ArbBinH a b -> Bool
igEstrucArb (Hoja a) (Hoja b) = True
igEstrucArb (Nodo a si sd) (Hoja b) = False
igEstrucArb  (Hoja b) (Nodo a si sd)  = False
igEstrucArb  (Nodo a si sd)(Nodo b sii sdd) = (igEstrucArb si sii) && (igEstrucArb sd sdd)

rotuloHoja:: ArbBinH a b -> Maybe a
rotuloHoja (Hoja n) = Just n
rotuloHoja _ = Nothing

rotuloNodo:: ArbBinH a b -> Maybe b
rotuloNodo (Nodo n si sd) = Just n
rotuloNodo _ = Nothing


-- si hoja y nodo truvieran un rotulo del mismo tipo

-- data ArbBin a = Hoja a | Nodo a (ArbBin a) (ArbBin a) deriving Show

-- crearArbol :: a -> ArbBin a -> ArbBin a -> ArbBin a
-- crearArbol a si sd = Nodo a si sd

-- -- crearArbolVacio?
-- nroNodos :: ArbBin a -> Int
-- nroNodos (Hoja n) = 0
-- nroNodos (Nodo a si sd) = 1+ (nroNodos si) + (nroNodos sd)

-- nroHojas :: ArbBin a -> Int
-- nroHojas (Hoja n) = 1
-- nroHojas (Nodo a si sd) = (nroHojas si) + (nroHojas sd)

-- altura:: ArbBin a -> Int
-- altura (Hoja n) = 1
-- altura (Nodo a si sd) = 1 +  maximo (altura si) (altura sd)

maximo:: Int -> Int -> Int
maximo n m = if n > m then n else m

-- preorden:: ArbBin a -> [a]
-- preorden (Hoja r) = [r]
-- preorden (Nodo r si sd) = [r] ++ (preorden si) ++ (preorden sd)

-- inorden:: ArbBin a -> [a]
-- inorden (Hoja r) = [r]
-- inorden (Nodo r si sd) =  (inorden si) ++ [r] ++ (inorden sd)

-- posOrden:: ArbBin a -> [a]
-- posOrden (Hoja r) = [r]
-- posOrden (Nodo r si sd) =  (posOrden si) ++ (posOrden sd) ++ [r] 

-- igEstrucArb :: ArbBin a -> ArbBin a -> Bool
-- igEstrucArb (Hoja a) (Hoja b) = True
-- igEstrucArb (Nodo a si sd) (Hoja b) = False
-- igEstrucArb  (Hoja b) (Nodo a si sd)  = False
-- igEstrucArb  (Nodo a si sd)(Nodo b sii sdd) = (igEstrucArb si sii) && (igEstrucArb sd sdd)

-- 10
data ArbBinRotHoj a = HojaRot a | NodoSinRot (ArbBinRotHoj a) (ArbBinRotHoj a) deriving Show

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

-- otra forma
-- nroNodosGen (HojaGen n) = 0
-- nroNodosGen (NodoGen a xs) = sum(map nroNodosGen xs)

nroHojasGen :: ArbGen a -> Int
nroHojasGen (HojaGen n) = 1
nroHojasGen (NodoGen n (a:as)) = (nroHojasGen a) + (nroHojasGen2 as)

nroHojasGen2 :: [ArbGen a] -> Int
nroHojasGen2 [] = 0
nroHojasGen2 (a:as) = (nroHojasGen a) + (nroHojasGen2 as)

-- otra forma
-- nroHojasGen (NodoGen _ []) = 1
-- nroHojasGen (NodoGen _ xs) = sum (map nroHojasGen xs) 

maxl :: [Int] -> Int --suponemos que es una lista de enteros
maxl (a:[]) = a
maxl (a:as) = if a> maxl as then a else maxl as

alturaGen:: ArbGen a -> Int
alturaGen (HojaGen n) = 1
alturaGen (NodoGen n (a:as)) = 1 +  maxl ((alturaGen a) :(alturaGen2 as))
-- alturaGen (NodoGen _ xs) = 1 + maxl (map alturaGen xs)

alturaGen2:: [ArbGen a] -> [Int]
alturaGen2 [] = []
alturaGen2 (a:as) =  ((alturaGen a) : (alturaGen2 as))

preordenGen:: ArbGen a -> [a]
preordenGen (HojaGen r) = [r]
-- preordenGen (NodoGen r (a:as)) = [r] ++ (preordenGen a) ++ (preordenGen2 as)
preordenGen (NodoGen r as) = [r] ++ concatMap preordenGen as

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
-- posordenGen (NodoGen r (a:as)) =  (posordenGen a) ++  (posordenGen2 as) ++ [r]
posordenGen (NodoGen r as) =  concatMap preordenGen as ++ [r]


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
-- igEstrucArbGen  (NodoGen a (x:xs)) (NodoGen b (y:ys)) = (long xs) == (long ys) && (igEstrucArbGen2 xs ys)
igEstrucArbGen  (NodoGen a xs) (NodoGen b ys) = (long xs) == (long ys) && and (zipWith igEstrucArbGen xs ys)


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
type Matriz a = [[a]]
sumarDosListas:: Num a => [a] -> [a] -> [a]
sumarDosListas [] ys = ys
sumarDosListas xs [] = xs
sumarDosListas (x:xs) (y:ys) = ((x + y):(sumarDosListas xs ys))

sumaMat:: Num a => Matriz a -> Matriz a -> Matriz a
sumaMat [] yss = yss
sumaMat xss [] = xss
sumaMat (xs:xss) (ys:yss) = ((sumarDosListas xs ys): (sumaMat xss yss))

trasp:: Matriz a -> Matriz a
trasp ([]:_) = []
trasp mat =  map head mat : trasp (map tail mat)

productoMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
productoMatrices a b =
  let bt = trasp b
  in map (\fila -> map (\col -> sum (zipWith (*) fila col)) bt) a

-- 14 Definir el tipo secuencia de numeros 
data Seq a = Seq [a] Int deriving (Eq, Show)

crearSecVac:: a -> Seq a
crearSecVac n = Seq [] 0

regAct :: Int -> Seq a -> Seq a
regAct n (Seq xs _)
  | n < 1 || n > length xs = error "Error: número de registro inválido"
  | otherwise = (Seq xs (n - 1))


leerReg :: Seq a -> (Maybe a, Seq a)
leerReg (Seq xs p) = if p >= length xs then (Nothing, Seq xs p) else (Just (xs!!p), Seq xs (p+1))

primeroSeq :: Seq a -> a
primeroSeq s1 = v1 where (Just v1,s2) = leerReg s1

segundoSeq :: Seq a -> a
segundoSeq s1 = v2 where (Just v1,s2) = leerReg s1
                         (Just v2,s3) = leerReg s2

borrReg :: Seq a -> Seq a
borrReg (Seq _ (-1)) = error "Error: no hay registro activo para borrar"
borrReg (Seq xs i)
  | i < 0 || i >= length xs = error "Error: índice fuera de rango"
  | otherwise =
      let xs' = take i xs ++ drop (i + 1) xs
       in (Seq xs' (-1))

agReg :: a -> Seq a -> Seq a
agReg x (Seq xs _) = Seq (xs ++ [x]) (-1)

numAct :: Seq a -> Int
numAct (Seq _ (-1)) = error "Error: no hay registro activo"
numAct (Seq _ i) = (i + 1)


-- 15 Definir el tipo ConjInf (conjuntos posiblemente infinitos) a partir de funciones características (dado un dato dice si pertenece o no al conjunto). Definir funciones similares a las del tipo conjunto (sin la función esVacio).
data ConjInf a = ConjInf (a-> Bool)
pertenece:: ConjInf a -> a -> Bool
pertenece (ConjInf f) x = f x

unionConjInf :: ConjInf a -> ConjInf a -> ConjInf a
unionConjInf (ConjInf f1) (ConjInf f2) = ConjInf (\x -> f1 x || f2 x)

interseccionConjInf :: ConjInf a -> ConjInf a -> ConjInf a
interseccionConjInf (ConjInf f1) (ConjInf f2) = ConjInf (\x -> f1 x && f2 x)

-- 16i Definir el tipo de dato GNO (grafo no orientado) con sus constructores.
type Nodo = Int
type Arco = (Nodo, Nodo)

data GNO= Grafo [Nodo] [Arco] deriving (Eq, Show)

-- 16ii Definir la función esArbol, que dice si un grafo cumple con la condición de ser árbol. Asumir definida la función existeCamino, que dados dos nodos y un GNO, 
-- devuelve si el segundo nodo es accesible desde el primero a través de un camino de arcos del GNO dado de longitud mayor o igual que uno.
esArbol::GNO -> Bool
esArbol (Grafo nodos aristas) = esConexo (Grafo nodos aristas) && length aristas == length nodos - 1

esConexo :: GNO -> Bool
esConexo (Grafo [] _) = True
esConexo (Grafo [_] _) = True
esConexo (Grafo nodos arcos) =
  all (\(x, y) -> x == y || existeCamino x y (Grafo nodos arcos)) pares
  where
    pares = [(x, y) | x <- nodos, y <- nodos, x < y]

-- esConexo =   all ( \nodo -> existeCamino nodo head(tail nodos) (Grafo nodos arcos)) nodos

-- asumir que existe existeCamino::Nodo -> Nodo -> GNO -> Bool
existeCamino :: Nodo -> Nodo -> GNO -> Bool
existeCamino origen destino grafo@(Grafo nodos aristas)
  | origen == destino = False  -- No queremos caminos de longitud 0
  | otherwise = destino `elem` dfs origen []
  where
    dfs :: Nodo -> [Nodo] -> [Nodo]
    dfs actual visitados
      | actual `elem` visitados = []
      | otherwise =
          let vecinos = vecinosDe actual grafo
              nuevos = filter (`notElem` visitados) vecinos
          in actual : concatMap (\n -> dfs n (actual : visitados)) nuevos

vecinosDe :: Nodo -> GNO -> [Nodo]
vecinosDe n (Grafo _ aristas) =   [y | (x, y) <- aristas, x == n] ++   [x | (x, y) <- aristas, y == n]

--  19) i) Describir el tipo GR (gramática regular con producciones lambda) con terminales de cualquier tipo.
-- data Produccion a b = Lambda | NTT b b a | T a | TNT b a b
-- data GR a b = Gram [a] [b] b [Produccion a b] deriving Show

-- otra opcion

type NoTerminal = Char

data Produccion a
  = Termina a NoTerminal    -- X → a Y
  | Final a                 -- X → a
  | Lambda                  -- X → λ
  deriving (Eq, Show)

type GR a = ([NoTerminal], [a], NoTerminal, [(NoTerminal, Produccion a)])

--19) ii) Programar la función currificada esInfinito, que decida si el lenguaje generado por una gramática regular es infinito o no. Asumir que existe una 
-- función esAlcanzable, que dado un no terminal y una gramática regular, diga si genera algún elemento a partir del no terminal dado de la gramática regular dada.
esInfinito :: Eq a => GR a -> Bool
esInfinito (nts, ts, s, prods) = any (\ n -> esAlcanzable n (nts, ts, s, prods)) prods
 

esAlcanzable :: Eq a => NoTerminal -> GR a -> Bool


-- 21) i) Programar el tipo de dato Expresion que permite representar expresiones aritméticas enteras con operadores de suma, resta, multiplicación, división, negación unaria y elevación al cuadrado.

data Expresion = Const Int | Suma Expresion Expresion | Resta Expresion Expresion | Mult Expresion Expresion  | Div Expresion Expresion | Neg Expresion | Pot Expresion deriving (Show, Eq)

-- 21) ii Programar una función listaAExp que dada una lista de cadenas de caracteres que representa una expresión correctamente escrita en notación prefija, devuelva la expresión asociada de tipo expresion.

listaAExp:: [String] -> Expresion
listaAExp xs = fst (parseExp xs)
  

parseExp:: [String] -> (Expresion, [String])
parseExp (x:xs) 
  | x == "+" = let (e1, r1) = parseExp xs
                   (e2, r2) = parseExp r1
               in (Suma e1 e2, r2)
  | x == "-" = let (e1, r1) = parseExp xs
                   (e2, r2) = parseExp r1
               in (Resta e1 e2, r2)
  | x == "*" = let (e1, r1) = parseExp xs
                   (e2, r2) = parseExp r1
               in (Mult e1 e2, r2)
  | x == "/" = let (e1, r1) = parseExp xs
                   (e2, r2) = parseExp r1
               in (Div e1 e2, r2)
  | x == "neg" = let (e, r) = parseExp xs
                 in (Neg e, r)
  | x == "cuad" = let (e, r) = parseExp xs
                  in (Pot e, r)
  | all (`elem` "-0123456789") x = (Const (read x), xs)
  | otherwise = error ("Símbolo no reconocido: " ++ x)


-- 21)iii Programar además la función inversa de la anterior (expALista), que dada una expresión del tipo expresión, devuelve una lista con el formato antes visto.
-- Nota:  Asumir que existe una función strAInt, que dado un número en formato de lista de char devuelve un Int, y su inversa intAStr. No programar estas funciones.

expALista:: Expresion -> [String]
-- expALista (Const n)        = [(read n)::Int]
expALista (Suma e1 e2)     = ["+"] ++ expALista e1 ++ expALista e2
expALista (Resta e1 e2)    = ["-"] ++ expALista e1 ++ expALista e2
expALista (Mult e1 e2)     = ["*"] ++ expALista e1 ++ expALista e2
expALista (Div e1 e2)      = ["/"] ++ expALista e1 ++ expALista e2
expALista (Neg e)          = ["-u"] ++ expALista e
expALista (Pot e)     = ["^"] ++ expALista e