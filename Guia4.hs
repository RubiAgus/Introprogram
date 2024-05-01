


--Ejercicio 1, Secuencia de Fibonacci
fib :: Int -> Int 
fib x  
    | x == 0 = 0
    | x == 1 = 1 
    |otherwise = fib(x-1) + fib (x-2)

fibList :: Int -> [Int] 
fibList x = map fib [0..x-1]

--Implementar una función parte Entera

parteEntera:: Float -> Int
parteEntera  0 = 0
parteEntera x --requiere un caso base, si 0<=x<1
    |x >= 0 && x < 1 = 0
    |x > 1 = parteEntera (x-1) + 1
    |x < 1 = parteEntera (x+1) -1


esDivisible:: Int -> Int  -> Bool  --No utilizar mod o div.
esDivisible x y
    | x < y = False
    | x == y = True 
    | otherwise = esDivisible (x-y) y

--para que sea divisible, un número se puede definir como la multiplicación de
-- 2 numeros/ 10/5 = 2 <=> 2 * 5 = 10 . Es divisible 10 por 5 también sabés 
--para que sea divisible, 0 = 10 -5 * x <=> 10 es divisible por 5


--ejc 4. suma impares 3 = 1+3+5 (caso base 1 = 1)
sumaImpares :: Int -> Int 
sumaImpares (x) 
    | x == 1 = 1
    | mod x 2 == 0 = sumaImpares (x+1) 
    | otherwise = x + sumaImpares (x-2) 
    -- ese "x" solo es importante, es para que se acumule

--n!! = n (n-2) (n-4)...
--caso base n = 0, o  el n es par / si n = 2 => 2
medioFac:: Int -> Int
medioFac 0 = 1
medioFac 1 = 1
medioFac x 
    |mod x 2 == 0 =  x * medioFac (x-2)
    |otherwise = x * medioFac (x-2) 

--Ejc 6 
quitarDigitos:: Int -> Int
quitarDigitos z = div z 10

sumaDigitos :: Int -> Int 
sumaDigitos x 
    | x <= 9  = x 
    | otherwise = mod x 10 + sumaDigitos(quitarDigitos x)
--es capicua
dejarUnSoloDigito::Int -> Int 
dejarUnSoloDigito n 
    | n <10 = n
    |otherwise = dejarUnSoloDigito (quitarDigitos n)
esCapicua :: Int -> Bool
esCapicua x = mod x 10 == dejarUnSoloDigito x --mal el concepto
--Ejercicio 10 
funcion1:: Int -> Int 
funcion1 x 
    | x == 0 = 1
    |otherwise = 2^x + funcion1 (x-1)

 --Ejc 11
--Factorial
factorial:: Int -> Float
factorial 0 = 1
factorial n = fromIntegral n* factorial (n-1)

eaprox :: Int -> Float
eaprox n
    | n == 0 = 1
    |otherwise = 1 /(factorial n)  + eaprox (n - 1)
e = eaprox 10
--Ejc 12
raiz2:: Int -> Float
raiz2 1 = 1
raiz2 x = ( raiz2(x-1) + 2 /raiz2(x-1)) / 2

-- Espicificar e implementar la siguiente 
-- funcion f(n,m) =  sum n i=1  sum m j=1 i^j 
--n <= 0 = 1 convencion, m <= 0 = 1

sumatoriaCompuesta:: Int -> Int -> Int
sumatoriaCompuesta n m
    | n <= 0 = 0
    | m <= 0 = 1
    | m == 1 = n 
    |otherwise = n^m + sumatoriaCompuesta (n-1) (m-1)

--ejc 16
menorDivHasta:: Int -> Int -> Int
menorDivHasta n i 
    |mod n i == 0 = i
    |otherwise = menorDivHasta n (i+1)
menorDiv :: Int -> Int
menorDiv n
    |n == 1 = n
    |otherwise = menorDivHasta n 2

esPrimo:: Int -> Bool
esPrimo x 
    |x == 1 = False
    |menorDiv x == x = True
    |otherwise = False 

--ejc 16 c si no tienen algún divisor común entre ambos
-- sonCoprimos:: Int -> Int -> Bool
-- sonCoprimos x y 
--     | mod x y == y || mod y x == x = True
--     |otherwise = False

-- contadorPrimos:: Int -> Int

