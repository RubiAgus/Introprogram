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