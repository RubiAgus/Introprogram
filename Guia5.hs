--Funcion Pertenece en listas
--Caso base, Lista vacia
pertenece::(Eq a) => a -> [a] -> Bool
pertenece _ [] = False
pertenece a  (y:ys)
    |a  == y = True
    |otherwise = pertenece a (ys)

--en clase

-- belongs::(Eq a) => a -> [a] -> Bool
-- belongs _ [] = False --Caso base
-- belongs a (x:xs) = a == x || belongs a xs

--2.4, Hay repetidos
--Caso base, la lista está vacía (NO HAY REPETIDOS)

hayRepetidos:: (Eq t) => [t] -> Bool
hayRepetidos []= False
hayRepetidos (l:ls)

    | pertenece l ls = True
    | otherwise = hayRepetidos (ls)
