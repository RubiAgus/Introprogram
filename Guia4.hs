--Ejercicio 1, Secuencia de Fibonacci
fib :: Int -> Int 
fib x  
    | x == 0 = 0
    | x == 1 = 1 
    |otherwise = fib(x-1) + fib (x-2)

fibList :: Int -> [Int] 
fibList x = map fib [0..x-1]
