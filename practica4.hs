-- 4) Generar la lista de los primeros mil números perfectos. Un número natural n es perfecto si la suma de sus divisores menores estrictos que él es igual a n. 
-- Ver de qué forma se puede realizar mejor usando evaluación lazy. Intentar programarla usando funciones de orden superior.
numerosPerfectos:: [Int]
-- -- lo primero que pense:
-- -- numerosPerfectos = fst (while (\ (actual, resto) -> (length actual) < 1000) (\ (actual, (x:xs)) -> if esPerfecto x then ((actual ++ [x]), xs) else (actual, xs)) ([],[1..]))
numerosPerfectos = take 1000 [x | x <- [1..], esPerfecto x ]

while::(a -> Bool) -> (a -> a) -> a -> a
while c f n = if c n then (while c f (f n)) else n


(<->):: Int -> Int -> [Int]
x <-> n = if x /= n then x:((x + 1) <-> n) else [x]

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (a:xs) = a + sumaLista (xs) 

esPerfecto:: Int -> Bool
-- esPerfecto n = sumaLista ((filter (\ x -> (mod n x == 0)) ((<->) 1 (n - 1)))) == n
esPerfecto n = sum (filter (\x -> n `mod` x == 0) [1..(n `div` 2)]) == n -- para agilizar

-- pegar :: [[a]] -> [a]
-- pegar = flat
flat :: [[a]] -> [a]
flat [[]] = []
flat ((a:as):[[]]) = (a:as)
flat ([]:(b:bs)) = flat (b:bs)
flat (a:as) = [head(a)] ++ (flat((tail a):as))

intercalar :: [a] -> [a] -> [a]
intercalar [][] = []
intercalar [](a:as) = (a:as)
intercalar (a:as)[] = (a:as)
intercalar (a:as) (b:bs) = [a] ++ [b] ++ (intercalar as bs)

listaPares :: ([a], [b]) -> [(a,b)]
-- listaPares (xs,ys) = flat (map (\i -> (map (\j -> (i, j)) ys)) xs) -- eso seria el producto cartesiano
listaPares (xs, ys ) = zip xs ys
-- listaPares (xs, ys) = intercalar pares []
--   where
--     pares = zip xs ys