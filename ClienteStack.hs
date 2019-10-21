-- PRACTICA 4

--ejercicio 4

import Stack

-- Orden (n^2)
apilar :: [a] -> Stack a
apilar xs = apilarAux (reverseList xs)

--auxiliar apilar
apilarAux :: [a] -> Stack a
apilarAux [] = emptyS
apilarAux (x:xs) = push x (apilar xs)

--Orden
desapilar :: Stack a -> [a]
desapilar stack = if isEmptyS stack 
    then []
    else reverseList (