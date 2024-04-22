--Quitar 
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar a (x:xs) 
  | a  == x = xs
  |otherwise x: quitar a xs
