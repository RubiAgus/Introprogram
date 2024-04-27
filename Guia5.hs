--Funcion Pertenece en listas
pertenece ::(Eq a) =>  a -> [a] -> Bool
pertenece _ [] = False
pertenece b (y:ys)
    |b == y = True 
    |otherwise = pertenece b (ys)

longitud:: (Eq t) =>[t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

ultimo:: (Eq b) => [b] -> b
ultimo (x:xs) 
    | xs == [] = x
    | otherwise = ultimo xs

principio::(Eq t) => [t] -> [t] -- todos los elementos menos el último
principio (x:xs)  -- separo el primer elemento, quiero ver si xs es []
    |xs == [] = [x] -- si xs es [], devolveme x por que es el último elem
    |otherwise = principio (xs)


-- |otherwise = [head xs] ++ principio (tail xs) EJEMPLAZO CONCATENACION
-- Pero no es lo que pide el ejc

reverso::(Eq t) => [t] -> [t]  -- invertir la lista
reverso [x] = [x]
reverso (x:xs)
    | xs == [] = [x]
    |otherwise = reverso (xs) ++ [x]



--en clase 

-- belongs::(Eq a) => a -> [a] -> Bool
-- belongs _ [] = False --Caso base
-- belongs a (x:xs) = a == x || belongs a xs

-- --2.4, Hay repetidos
-- --Caso base, la lista está vacía (NO HAY REPETIDOS)

-- hayRepetidos:: (Eq t) => [t] -> Bool
-- hayRepetidos []= False
-- hayRepetidos (l:ls)

--     | pertenece l ls = True
--     | otherwise = hayRepetidos (ls)
    
-- --Quitar 
-- quitar :: (Eq t) => t -> [t] -> [t]
-- quitar _ [] = []
-- quitar a (x:xs) 
--   | a  == x = xs
--   |otherwise x: quitar a xs
