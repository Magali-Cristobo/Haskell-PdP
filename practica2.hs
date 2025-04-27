-- type ListaVacia = []
type Poste = [Int] -- es mas una pila que otra cosa


-- movimiento tiene disco -> poste -> poste
-- data Movimiento = (numero,posteInicial, posteFinal)
-- type TuplaVacia = (,,)
data Movimiento = Mov Poste Poste deriving Show -- creo q no hace falta el int, no importa cuantos elementos haya, siempre muevo de a uno

-- hanoi :: Int -> [Movimiento]
-- hanoi n = 


-- la formula es 2^n - 1
mover :: Poste -> Poste -> Movimiento
mover [] [] = Mov [] []
mover (a:as) [] = Mov as [a]
mover (a:as) (b:bs) = Mov as ([a] ++ (b:bs))

data Maybe2 a = Nada | Solo a deriving Show

cabeza :: [a] -> Maybe2 a
cabeza [] = Nada
cabeza (a:as) = Solo a -- es asi??