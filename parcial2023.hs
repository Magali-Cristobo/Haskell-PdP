-- 1 
triangularSuperior::[[a]] -> [[a]] -- todo lo que esta arriba de la diagonal principal
triangularSuperior ms = triangularSuperior2 ms 1

triangularSuperior2::[[a]] -> Int -> [[a]]
triangularSuperior2 [] c = []
triangularSuperior2 (m:ms) c = ((drop c m) : (triangularSuperior2 ms (c + 1)))

-- b)
triangularSuperiorB::[[a]] -> [[a]]
-- triangularSuperiorB mss = [ drop c (mss !! (c-1)) | c <- [1..length mss]]
-- triangularSuperiorB mss = map (\c -> drop c (mss !! (c-1)) ) [1..length mss]
triangularSuperiorB mss = zipWith drop [1..length mss] mss

-- c)
triangularSuperiorC mss = do { c <- [1..length mss]; return  (drop c (mss !! (c-1)))} 
triangularSuperiorD mss = [1..length mss] >>= ( \c -> return (drop c (mss !! (c-1)) )) -- el bind hace un concat, aplana las listas. Por eso sin el return nos devuelve una sola 

-- 3 i)
data ListaConBase a b = Base b | NoVacia a (ListaConBase a b) deriving (Eq, Show)

-- ii)
foldrListaConBase:: (a -> c -> c) -> (b -> c) -> (ListaConBase a b) -> c
foldrListaConBase f g (Base x) = g x
foldListaConBase f g (NoVacia y ys) = f y (foldrListaConBase f g ys)

-- iii)
type ListaNoVacia a = ListaConBase a a

-- iv)
foldrLNV::(a -> b -> b) -> b -> (ListaNoVacia a) -> b
foldrLNV f x ls = foldrListaConBase f (\u -> f u x) ls -- la u representa la segunda lista con base

-- Definir el tipo de map foldr [(!!)]
{-
map:: (a -> b) -> [a] -> [b]
foldr:: (a -> b -> b) -> b -> [a] -> b
(!!):: [a] -> [Int] -> a
[(!!)]::[[a] -> Int -> a ]

(map foldr):: unificar tipo de dominio de map con el tipo de foldr, retornando el tipo de imagen de map
dominio de map (x -> y), que es lo que recibe
entonces podemos sustitur x por (a -> b -> b)  e y  por b -> [a] -> b
la imagen de map: [x] -> [y], 
aplicando sustituciones
map foldr: [a -> b -> b] -> [b -> [a] -> b]

para [(!!)] hay que unificar el tipo de dominio de map con el tipo de [(!!)], retornando el tipo de imagen de map foldr.
dominio de map foldr: [x -> y -> y]
entonces sustituimos x por  [a], y por Int, a por int (esto si o si porque x -> y -> y), entonces en realidad x = [Int]

la imagen de map foldr es: [y -> [x] -> y]
entonces sustituimos y por Int, x por [Int] (quedando [[Int]]).

quedaria entonces
map foldr [(!!)]:: [Int -> [[Int]] -> Int]
-}