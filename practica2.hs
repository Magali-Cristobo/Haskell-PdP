-- Definir la función hanoi, que dada una cierta cantidad de discos (mayor a cero), devuelva la lista de movimientos (de poste a poste) que deberían realizarse para resolver el juego de las torres de Hanoi. Especificar cómo son los tipos Poste y Movimiento.
type Poste = [Int] -- es mas una pila que otra cosa
type Movimiento = (Poste, Poste, Poste) -- (posteInicial, posteFinal) siempre se desapila el primero entonces saque el parametro de disco.

poste1 :: Poste
poste1 = [1,2,3,4,5]


mover :: Poste -> Poste -> Poste -> Movimiento
mover (a:as) [] [] = (as ,[],[a]) 
mover (a:as) (b:bs) (c:cs) = if a < c then (as, (b:bs),[a] ++ (c:cs)) else ((a:as),(b:bs), (c:cs))  -- no se si es correcto, pero es la idea

-- Otra forma, con Data: movimiento tiene disco -> poste -> poste
-- data Movimiento = (numero,posteInicial, posteFinal)
-- data Movimiento = Mov Poste Poste deriving Show -- creo q no hace falta el int, no importa cuantos elementos haya, siempre muevo de a uno

hanoi :: Int -> [Movimiento]
hanoi 0 = [([],[],[])]
hanoi n = [mover [x | x <- [1..n]] [] []] ++ [mover [x | x <- [2..n]] [] []] -- no funciona


-- la formula es 2^n - 1
-- mover :: Poste -> Poste -> Movimiento
-- mover [] [] = Mov [] []
-- mover (a:as) [] = Mov as [a]
-- mover (a:as) (b:bs) = if a < b then Mov as ([a] ++ (b:bs)) else Mov (a:as) (b:bs) -- no se si es correcto, pero es la idea


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
resta n m = if (esCero m) then n else Suc (resta (sucesor? n) m)
-- 5

-- data LambdaTerm = Crear [Lambda]
