-- Ej 13, sumatoria de sumatorias... F (n,m) = sumatoria de i hasta n de sumatoria de j hasta m de i^j 

sumatoriaCompuesta:: Int -> Int -> Int
sumatoriaCompuesta n m 
    | n <= 0 = 0 --Convension de sumatoria
    | m <= 0 = 1
    | m == 1 = n
    | otherwise = n^m + sumatoriaCompuesta n (m-1) 

--Ex 16 a
menorDivHasta :: Int -> Int -> Int 
menorDivHasta n i 
    |  mod n i == 0 =  i
    | otherwise = menorDivHasta n (i+1)
--use the function above inside the other function
menorDiv:: Int -> Int 
menorDiv n 
    | n == 1 = 1
    | otherwise = menorDivHasta n  2

--16 b)
esPrimo :: Int -> Bool
esPrimo x 
    |menorDiv x == x = True
    |otherwise = False 
-- primosLista:: [Int] -> Int
--     map. https://youtu.be/aTCeFgLRZbg?si=QBaXgxmxKg3PclnI Check that to see if it helps
--Es primo debe usar  el menor divisor para que el menor divisor sea 1.
--Ex 19, suma inicial de primos te dice si el numero dado puede formarse por una suma de los nùmeros primos en orden

     {-
esto probablemente sea asociado  a ver si x es primo, luego de eso voy a querer empezar sumandole 1, luego 3, luego 5, 7 and so on and so forth.
ARMÀ Una funcion que te sume una cantidad x de nùmeros primos en orden 
    -}
--Requiero un contador de nùmeros primos, para ir sumandolos
contadorPrimos:: Int -> Int
contadorPrimos x    
    | esPrimo x  == True
    |   othewise = False

-- sumaPrimos :: Int -> Int
-- sumaPrimos x --Suma x cantidad de primos
--     | x



-- sumaInicialDePrimos:: Int -> Bool
-- sumaInicialDePrimos n
--     | n = True
--     | otherwise = False




