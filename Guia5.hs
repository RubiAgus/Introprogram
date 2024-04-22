--Funcion Pertenece en listas
--Caso base, Lista vacíp
pertenece::(Eq a) => a -> [a] -> Bool
pertenece a  (y:ys)
    |ys == [] = False
    |a  == y = True
    |otherwise = pertenece a (ys)

--en clase 

belongs::(Eq a) => a -> [a] -> Bool
belongs _ [] = False --Caso base
belongs a (x:xs) = a == x || belongs a xs

--2.4, Hay repetidos
--Caso base, la lista está vacía (NO HAY REPETIDOS)
--Caso base 2, 1 elemento.
--caso base 3, 2 elementos iguales

hayRepetidos:: (Eq t) => [t] -> Bool
hayRepetidos []= False
hayRepetidos (l:ls)
 --ls == [] = False
    | pertenece l ls == True = True
    | otherwise = hayRepetidos ls
