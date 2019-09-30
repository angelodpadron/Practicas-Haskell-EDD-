--PRACTICA 4 Main
--2
import Set 

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] set = setToList set
losQuePertenecen [x] set = if belongs x set then [x] else []
losQuePertenecen (x:xs) set = losQuePertenecen [x] set ++ losQuePertenecen xs set

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos x = setToList (addA x emptyS)

addA :: Eq a => [a] -> Set a -> Set a
addA [] set = set
addA (x:xs) set = addS x (addA xs set)

--unirTodos :: Eq a => Tree (Set a) -> Set a
--unirTodos 