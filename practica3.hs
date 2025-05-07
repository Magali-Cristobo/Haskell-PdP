-- previo a la practica
sacaPares::[Int] -> [Int]
sacaPares [] = []
sacaPares (x:xs) = if(even x) then (sacaPares xs) else (x:(sacaPares xs))

-- version con funciones de orden superior
filterOS:: (a -> Bool) -> [a] -> [a]
filterOS f [] = []
filterOS f (x:xs)  = if(f x) then (x:(filterOS f xs )) else (filterOS f xs )

sacaParesOS:: [Int] -> [Int]
-- sacaParesOS = filterOS (\x -> odd x)
sacaParesOS = filterOS odd 


-- 1 Definir una función esCerrada, que dada una lista y una función binaria (que toma un par de elementos del tipo de la lista y devuelve un elemento 
-- del tipo de la lista) devuelve si la función es cerrada respecto a la lista de entrada (o sea, si la imagen de la función 
--tomando como dominio la lista-- está incluída en esta lista “dominio”).

esCerrada:: Eq a => [a] -> (a -> a  -> a) -> Bool
esCerrada xs f = and [ f x y `elem` xs | x <- xs, y <- xs]