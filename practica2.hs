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
-- 5

-- data LambdaTerm = Crear [Lambda]
