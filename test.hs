-- Ej 13, sumatoria de sumatorias... F (n,m) = sumatoria de i hasta n de sumatoria de j hasta m de i^j 

sumatoriaCompuesta:: Int -> Int -> Int
sumatoriaCompuesta n m 
    | n <= 0 = 0 --Convension de sumatoria
    | m <= 0 = 1
    | m == 1 = n
    | otherwise = n^m + sumatoriaCompuesta n (m-1) 

--Ex 16
menorDivHasta :: Int -> Int -> Int 
menorDivHasta n i 
    |  mod n i == 0 =  i
    | otherwise = menorDivHasta n (i+1)
--use the function above inside the other function
menorDiv:: Int -> Int 
menorDiv n 
    | n == 1 = 1
    | otherwise = menorDivHasta n  2
--esto llama a menorDivHasta y a i lo comienza en 2 y de ahì esto sigue tal que prueba todas las opciones hasta  llegar al que mod n i = o y te deberìa devolver i

--Ex 19, suma inicial de primos te dice si el numero dado puede formarse por una suma de los nùmeros primos en orden
esPrimo :: Int -> Bool
esPrimo x = True 
primosTest :: Int -> Bool
primosTest x = True
     {-
esto probablemente sea asociado  a ver si x es primo, luego de eso voy a querer empezar sumandole 1, luego 3, luego 5, 7 and so on and so forth.

    -}