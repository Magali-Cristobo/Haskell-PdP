-- previo a la practica
sacaPares::[Int] -> [Int]
sacaPares [] = []
sacaPares (x:xs) = if(even x) then (sacaPares xs) else (x:(sacaPares xs))

-- version con funciones de orden superior
filterOS:: (a -> Bool) -> [a] -> [a]
filterOS f [] = []
filterOS f (x:xs)  = if(f x) then (x:(filterOS f xs )) else (filterOS f xs )

-- otra forma 
{-
filter c (x:xs)
    | c x = x: filter c xs
    | otherwise = filter condicion xs

-}
sacaParesOS:: [Int] -> [Int]
-- sacaParesOS = filterOS (\x -> odd x)
sacaParesOS = filterOS odd 

-- map::(a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x : map f cola

-- all si todos los elementos de la lista cumplen un criterio 
-- all :: (a -> Bool) -> [a] - Bool
-- all _ [] = []
-- all condicion (x:xs) = condicion x && all condicion xs

hacerNVeces 0 f valor = valor
hacerNVeces n f valor | n > 0 = hacerNVeces (n-1) f (f valor)

-- 1 Definir una función esCerrada, que dada una lista y una función binaria (que toma un par de elementos del tipo de la lista y devuelve un elemento 
-- del tipo de la lista) devuelve si la función es cerrada respecto a la lista de entrada (o sea, si la imagen de la función 
--tomando como dominio la lista-- está incluída en esta lista “dominio”).

esCerrada:: Eq a => [a] -> (a -> a  -> a) -> Bool
esCerrada xs f = and [ f x y `elem` xs | x <- xs, y <- xs]

maxf:: Eq a => (a -> [a]) -> a
