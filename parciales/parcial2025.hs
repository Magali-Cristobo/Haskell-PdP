-- 1) i
mult3::Int -> [Int] -> [Int]
mult3 n xs = mult3b n 0 xs

mult3b::Int -> Int -> [Int] -> [Int]
mult3b n i [] = []
mult3b n i (x:xs)
    | x > n && mod i 3 == 1 = (x:mult3b n (i + 1) xs)
    | otherwise = mult3b n (i + 1) xs

-- 1)ii
mult3F::Int -> [Int] -> [Int]
mult3F n xs = snd (foldr (\ x (i,acc)-> if x > n && mod i 3 == 1 then ((i - 1), x:acc) else (i - 1, acc)) (length xs - 1 , []) xs)
-- justificacion: usamos foldr para recorrer la lista de derecha a izquierda, manteniendo un contador de índices y un acumulador de resultados.

-- 2) dadas las siguientes definciiones:
-- r h = h (r h)
-- g = r (\ f x y -> if null x then y else ((head x) : f (tail x) y ))
-- evaluar la expresion g [1,2,3] [4,0,1] en modo eager y normal order.
-- en el modo eager tenemos que evaluar todos los parametros antes de aplicar la funcion, como la funcion r es recursiva, se evaluaria hasta que se llegue a un constructor. La funcion r se define como fix.
-- normal order:
-- g [1,2,3] [4,0,1] = r (\ f x y -> if null x then y else ((head x) : f (tail x) y )) [1,2,3] [4,0,1]
-- = r (\ f x y -> if null x then y else ((head x) : f (tail x) y )) [1,2,3] [4,0,1]
-- = (\ f x y -> if null x then y else ((head x) : f (tail x) y )) (r (\ f x y -> if null x then y else ((head x) : f (tail x) y ))) [1,2,3] [4,0,1]
-- 
-- = [4,0,1]
-- 3)i
shuffle:: [a] -> [a] -> (Int -> Int -> a -> a -> Int -> Int -> Bool) -> [a]
shuffle xs ys f = shuffleb (length xs) (length ys) 0 0 xs ys f

shuffleb:: Int -> Int -> Int -> Int -> [a] -> [a] -> (Int -> Int -> a -> a -> Int -> Int -> Bool) -> [a]
shuffleb _ _ _ _ [] [] _= []
shuffleb _ _ _ _ [] ys _ = ys
shuffleb _ _ _ _ xs []  _= xs
shuffleb l1 l2 i j (x:xs) (y:ys) f = if (f l1 l2 x y i j) then (x: (shuffleb l1 l2 (i + 1) j xs (y:ys) f)) else  (y: (shuffleb l1 l2 i (j + 1) (x:xs) ys f))

-- ii)
intercalar:: Ord a => [a] -> [a] -> [a]
-- intercalar xs ys = shuffle xs ys (\ l1 l2 x y i j -> even (i + j))
intercalar xs ys = shuffle xs ys (\ l1 l2 x y i j -> i <= j)


-- iii)
merge::Ord a => [a] -> [a] -> [a]
merge xs ys = shuffle xs ys (\ _ _ x y _ _ -> (x <= y))

-- iv)
intercalarB:: Ord a => Int -> [a] -> [a] -> [a]
intercalarB n xs ys = shuffle xs ys (\ l1 l2 x y i j -> even (i + j) || i == n)
