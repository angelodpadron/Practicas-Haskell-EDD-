--PRACTICA 4 Main
--2
import Set 

data Tree a = NodeT a (Tree a) (Tree a) | EmptyT deriving (Show, Eq)

variableSet = addA [1,2,3,4,5,5] emptyS
variableSet2 = addA [5,6,7,8,9,15] emptyS

arbolSet = (NodeT variableSet EmptyT EmptyT)
arbolSet2 = (NodeT variableSet2 arbolSet EmptyT)

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] set = setToList set
losQuePertenecen [x] set = if belongs x set then [x] else []
losQuePertenecen (x:xs) set = losQuePertenecen [x] set ++ losQuePertenecen xs set

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos x = setToList (addA x emptyS)

addA :: Eq a => [a] -> Set a -> Set a
addA [] set = set
addA (x:xs) set = addS x (addA xs set)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos (EmptyT) = emptyS
unirTodos (NodeT s ti td) = unionS (unionS s (unirTodos ti)) (unirTodos td)