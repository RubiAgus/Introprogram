-- Ej 13, sumatoria de sumatorias... F (n,m) = sumatoria de i hasta n de sumatoria de j hasta m de i^j 

sumatoriaCompuesta:: Int -> Int -> Int
sumatoriaCompuesta n m 
    | n <= 0 = 0 --Convension de sumatoria
    | m <= 0 = 1
    | m == 1 = n
    | otherwise = n^m + sumatoriaCompuesta n (m-1) 
