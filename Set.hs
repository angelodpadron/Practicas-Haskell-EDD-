-- PRACTICA 4 Modulo


--2
module Set (
    emptyS, 
    addS, 
    belongs, 
    --sizeS,
    --removeS,
    --unionS,
    --intersectionS,
    --setToList,
) where

-- "a" es un entero positivo y unico en la lista
-- Int es igual a la cantidad de elementos del set (tamaño... ok?)

data Set a = UnSet [a] Int deriving (Show, Eq)

--Orden (1)
emptyS :: Set a
emptyS = UnSet [] 0

-- Orden (n)
addS :: Eq a => a -> Set a -> Set a
addS a (UnSet xs size) =
    if elem a xs
        then (UnSet xs size)
        else (UnSet (a:xs) (size+1))


belongs :: Eq a => a -> Set a -> Bool
belongs a (UnSet xs i) = elem a xs

