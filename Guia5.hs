module Guia5 where

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
--Ejercicio 2

--Funcion Pertenece en listas
pertenece ::(Eq a) =>  a -> [a] -> Bool
pertenece _ [] = False
pertenece b (y:ys)
    |b == y = True 
    |otherwise = pertenece b (ys)
-- Todos Iguales

todosIguales::(Eq t)=> [t] -> Bool-- todos los elementos son iguales?
todosIguales [] = True
todosIguales (x:xs)
    | xs == [] = True
    |pertenece x (xs) = todosIguales xs
    |otherwise = False 

todosDistintos:: (Eq t)=> [t] -> Bool
todosDistintos [] = True -- Caso base, voy a ir elemento a elemento
--comparando cada elemento con el resto de la lista usando pertenece
todosDistintos (x:xs)
    |xs == [] = True
    |pertenece x xs = False
    |otherwise = todosDistintos xs

hayRepetidos::(Eq t)=> [t] -> Bool
hayRepetidos ys 
    |todosDistintos ys == True = False
    |otherwise = True

--2.5 funcion quitar
quitar:: (Eq t)=> t -> [t] -> [t] -- quita un elemento elegido una sola vez en
quitar _ [] = []
quitar a (x:xs)
    | a == x = xs  
    | otherwise = [x] ++ quitar a xs

--2.6 quitar todos, quita toda aparición de un elemento determinado
quitarTodos:: Eq t => t -> [t] -> [t]
quitarTodos _ []=[] 
quitarTodos b (y:ys) 
    | b == y  = quitarTodos b ys
    |otherwise = y : quitarTodos b ys


{- si x está repetido, concateno x con el resto de la lista
eliminando x, llamo de vuelta a la función -}
eliminaRepetidos :: (Eq t) => [t] -> [t]
eliminaRepetidos [] = []
eliminaRepetidos (x:xs) 
    |pertenece x xs = x : eliminaRepetidos (quitarTodos x xs)
    |otherwise = [x] ++ eliminaRepetidos xs

--mismos elementos 
--requiero una función que verifique una función dentro de la otra
perteneceLista::(Eq t)=> [t] -> [t] -> Bool
perteneceLista [] _ = True --Caso base, la primera lista debe ser menor
perteneceLista (x:xs) ys 
    |pertenece x ys  = perteneceLista xs ys
    |otherwise = False

listLength::(Eq t)=> [t] -> Int
listLength []= 0
listLength (x:xs)= 1 + listLength xs

mismosElementos::(Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos as bs =
    perteneceLista (eliminaRepetidos as) (eliminaRepetidos bs)

capicua:: (Eq t)=> [t] -> Bool
capicua xs  = xs == reverso xs

--Ejercicio 3

sumatoria:: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs)= x + ( sumatoria xs)

productoria:: [Int] -> Int
productoria []= 1
productoria (x:xs)= x * productoria xs


extraerNumero :: [Int] -> Int
extraerNumero [x]= x

maximoLista::[Int]-> Int
maximoLista [b] = b
maximoLista (x:y:xs)
    | x >= y =  maximoLista (x:xs)
    |otherwise = maximoLista (y:xs)

sumarN:: Int -> [Int] -> [Int]
sumarN _ [] = []
sumarN z [x]= [z +x ]
sumarN z (x:xs) = [z + x] ++ sumarN z xs

ordenar::[Int]-> [Int]
ordenar [] = []
ordenar (xs) = ordenar((quitar(maximoLista xs)xs)) ++ [maximoLista xs]

--ejc 4, importante
sacarBlancosRepetidos::[Char]->[Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (y:x:xs)
    |y == x && x == ' ' = sacarBlancosRepetidos (x:xs)
    |otherwise = y:sacarBlancosRepetidos(x:xs)
-- Contar Palabras, dada una lista de characteres

--para contar palabras me conviene  contar espacios.
--y tambièn asegurarme de que el espacio inicial no sea espacio
quitarEspacioFinal::[Char]-> [Char]
quitarEspacioFinal [] = []
quitarEspacioFinal (x:[])
    | x== ' ' = []
    |otherwise = [x]
quitarEspacioFinal(x:xs)= x:quitarEspacioFinal xs

quitarEspacioIniFin::[Char]->[Char]
quitarEspacioIniFin [] = []
quitarEspacioIniFin(x:xs)
    |x== ' ' = quitarEspacioFinal xs
    |otherwise =x:quitarEspacioFinal xs-- there was an issue on the code above

contarEspacios::[Char]->Int
contarEspacios [] = 0
contarEspacios  (x:xs)
    |x == ' ' = 1 + contarEspacios xs
    |otherwise = contarEspacios  xs

quitarTodoEspacioAlPedo::[Char]->[Char]
quitarTodoEspacioAlPedo (xs) =quitarEspacioIniFin(sacarBlancosRepetidos xs)

contarPalabras::[Char]-> Int
contarPalabras xs =contarEspacios(quitarTodoEspacioAlPedo xs) + 1

--Lista de palabras
primeraPalabraAux::[Char]->[Char]
primeraPalabraAux [] = []
primeraPalabraAux (x:xs) 
    |x /= ' ' =  x :primeraPalabraAux (xs)
    |otherwise = []


primeraPalabra xs = primeraPalabraAux(quitarTodoEspacioAlPedo(xs))


quitarPrimeraPalabra::[Char]->[Char]
quitarPrimeraPalabra [] = []
quitarPrimeraPalabra (x:xs)
    |x == ' ' = xs
    |otherwise =  (quitarPrimeraPalabra xs)



palabrasAUX::[Char]->[[Char]]
palabrasAUX [] = []
palabrasAUX (x:xs) = primeraPalabra (x:xs):palabrasAUX(quitarPrimeraPalabra (x:xs))

palabras xs = palabrasAUX(quitarTodoEspacioAlPedo xs)


palabraMasLarga::[Char]->[Char]
palabraMasLarga []= []
palabraMasLarga (xs)
    |quitarPrimeraPalabra (xs) == [] = primeraPalabra(xs)
    |longitud (primeraPalabraAux xs) > longitud(primeraPalabra(quitarPrimeraPalabra xs)) = palabraMasLarga(
        primeraPalabra xs ++ ' ':(quitarPrimeraPalabra(quitarPrimeraPalabra xs)))    
    |otherwise = palabraMasLarga (quitarPrimeraPalabra xs)

aplanar::[[Char]] -> [Char]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar (xs)

aplanarConBlancos:: [[Char]] -> [Char]
aplanarConBlancos [] = []
aplanarConBlancos (x:xs) = x ++ ' ':aplanarConBlancos (xs)

addEspacios::Int -> [Char]
addEspacios 0 = []
addEspacios num  =  ' ': addEspacios(num-1) 
--n requiere que yo haga una secuencia de blancos
aplanarConNBlancos::[[Char]]->Int ->[Char]
aplanarConNBlancos [] _ = []
aplanarConNBlancos (x:xs) y 
    |xs /=[] = x ++ (addEspacios y) ++ aplanarConNBlancos xs y
    |otherwise = x ++ aplanarConNBlancos xs y

sumaAcumulada::[Int]->[Int]
sumaAcumulada []=[]
sumaAcumulada [x]=[x]
sumaAcumulada (x:y:xs) = x:(sumaAcumulada(y+x:xs))

