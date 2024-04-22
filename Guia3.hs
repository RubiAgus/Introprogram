-- excercise 1
{-
Implentar la funci´on parcial f :: Integer -> Integer definida por
extensi´on de la siguiente manera:
f(1) = 8, f(4) = 131, f(16) = 16
-}

ejc1A:: Int -> Int
ejc1A x 
    | x == 1 = 8
    | x == 4 = 131
    | x == 16 = 16 

-- excercise  1 b, same shtick G(x), instead
ejc1B :: Int -> Int
ejc1B x
    | x == 8 = 16
    | x == 4 = 16
    | x == 131 = 1
   
{-
A partir de las funciones definidas en los ´ıtems 1 y 2, implementar las
funciones parciales h = f ◦ g y k = g ◦ f, F(G(x))
composcicion de funciones.
-}

 
compuesta = ejc1B.ejc1A-- Here you are telling it to use the function B and then run it inside A

compuesta2 = ejc1A.ejc1B -- Not necessary to type the inputs because you declared it previously

--Ejercicio 2 a)
absoluto:: Int -> Int 
absoluto x
    | x >= 0 = x
    | otherwise  = (-x)
-- Existe la funcion abs que ya hace esto

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y 
    | absoluto x >= absoluto y = absoluto x
    | otherwise = absoluto y

-- 2 c)
maxnum3 :: Int -> Int -> Int -> Int 
maxnum3 x y z  = max x  (max y z)

--2 d) Alguno es 0, I will take the result as boolean
algunoEs0 :: Int -> Int -> Bool 
algunoEs0 x y = x == 0 || y == 0

-- 2 e) 
ambosSon0 :: Int -> Int -> Bool
ambosSon0 n m = n == 0 && m == 0

-- 2 f) mismo intervalo
mismoIntervalo :: Int -> Int -> Bool
mismoIntervalo x y 
        | x <= 3 && y <= 3 = True
        | x >= 7 && y >= 7 = True
        | x > 3 && x < 7 && y > 3 && y < 7 = True
        |otherwise = False
-- 2 g) Suma distintos

sumaDistintos :: Int -> Int -> Int -> Int 
sumaDistintos x y z 
    | x /= y  && x /= z && y /= z = x + y + z
   | x == y = z
   | x == z = y
   | y == z = x
   | otherwise = 0
--2 h) Dado 2 numeros naturales decidir si el primero es multiplo del segundo
esMultiploDe :: Int -> Int -> Bool 
esMultiploDe x y 
    |mod y x == 0 = True
    |otherwise = False

--2 i) dado un numero entero, extraer el digito de las unidades
-- div 10
quitarUnidades :: Int -> Int
quitarUnidades x 
    | x >= 0 = div x 10
    | otherwise = -quitarUnidades (-x)
-- div 999 100 = 9, con eso quiero quitar las decenas pero devolver las unidades
quitarDecenas :: Int -> Int 
quitarDecenas n 
    | n >= 0  = (div n 100) * 10 + mod n 10
    | otherwise = -quitarDecenas (-n)

{-
No estaba logrando que el ejercicio me de con uno negativo y se
que habia una manera distinta de hacerlo en clase pero en el pastebin esta
hecho asi: 
sacarUnidades :: Int -> Int 
sacarUnidades x | x < 0 = -sacarUnidadesPos (-x)
                | otherwise = sacarUnidadesPos x
-}

--EJERCICIO 3  a*a + a*b * k = 0

estanRelacionados :: Int -> Int -> Bool
estanRelacionados x y = mod (x * x) y == 0  

--Ejercicio 4
-- 4 a) producto entre 2 tuplas  de reales en reales

prodInterno:: (Float,Float) -> (Float,Float) -> (Float,Float)
prodInterno (x,y) (n,m) = (x*n, m*y)

--4 b) 
todoMenor:: (Float,Float) -> (Float,Float) -> Bool
todoMenor (x,y) (z,u) = x < z && y < u

-- 4 c) distancia entre dos puntos de R2
distanciaTuplas :: (Float,Float) -> (Float,Float) -> Float
distanciaTuplas (w, x) (y,z) = sqrt((y-w) * (y-w) +(z-x)*(z-x))

-- 4 d)
sumaTerna :: (Int,Int,Int) -> Int
sumaTerna (x, y , z) = x + y + z

--4 e) Sumar Solo Multiplos
esMultiplo0 :: Int -> Int -> Int
esMultiplo0 k x
    | esMultiploDe x k == True = k
    | otherwise = 0

sumaSoloMultiplos:: (Int, Int, Int)-> Int -> Int
sumaSoloMultiplos (x,y,z) n =  esMultiplo0 x n + esMultiplo0 y n + esMultiplo0 z n
--no se como pedirle que lo haga sin poner 8 combinaciones distintas


--4 f) devuelve la posicion del numero par
posPrimerPar :: Int -> Int ->Int -> Int
posPrimerPar x y z
    | mod x 2 == 0 = 1
    | mod y 2 == 0 = 2
    |mod  z 2 == 0 = 3
    |otherwise = 4
-- 4 g) Juntar dos Variables en una tupla, no importa el tipo de var

crearPar :: a -> b -> (a,b)
crearPar a b = (a,b) 
--4 h) 
invertir ::(a,b) -> (b,a)
invertir (a,b) = (b,a)

-- Ejercicio 5
--problema f
problemaF :: Int -> Int
problemaF x 
    | x <= 7 = x * x
    | otherwise  = 2x - 1
--problema g
problemaG :: Int -> Int 
problemaG x 
    | mod x 2 == 0 = div x 2
    | otherwise = 3x+1
    