import Guia5 
import Test.HUnit

-- testLength = [
--     "Caso base 1: listaVacia"~:(longitud ([]::[Char])) ~?= 0,
--     "Caso base 2: 1 elemento"~:(longitud ['a']) ~?= 1,
--     "Caso 3 elementos"~:(longitud[1,3,5]) ~?= 3

--     ]

testLength = test [
    "Empty list" ~: longitud ([] :: [Int]) ~?= 0,
    "Single element" ~: longitud [42] ~?= 1,
    "Multiple elements" ~: longitud [1, 2, 3, 4, 5] ~?= 5,
    "List of characters" ~: longitud "hello" ~?= 5, 
    "List of booleans" ~: longitud [True, False, True] ~?= 3
    ]

testPrimeraPalabra = test [
    "listaVacia"~: primeraPalabra [] ~?= [],
    "lista con espacio adelante"~: primeraPalabra " working? xD" ~?= "working?"
    ]

testQuitarPrimeraPalabra = test [
    "Lista Vacía"~: quitarPrimeraPalabra [] ~?= [],
    "lista una palabra"~: quitarPrimeraPalabra "hola" ~?= [],
    "lista 3 palabras"~: quitarPrimeraPalabra  "hola todo bien?" ~?= "todo bien?"

    ]
    
testPalabrasAUX = test [
    "Lista Vacía"~:  palabras[] ~?= [],
    "lista una palabra"~: palabras "Hola" ~?= ["Hola"],
    "lista 3 palabras"~: palabras "hola todo bien?" ~?= ["hola","todo", "bien?"],
    "lista espacios de sobra"~: palabras " hola  todo    bien?" ~?= ["hola","todo", "bien?"]

    ]

