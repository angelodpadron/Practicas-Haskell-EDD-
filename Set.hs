-- PRACTICA 4 Modulo


--2
module Set (
    Set,
    emptyS, 
    addS, 
    belongs, 
    sizeS,
    removeS,
    unionS,
    interseccionS,
    setToList,
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


-- Orden (n)
belongs :: Eq a => a -> Set a -> Bool
belongs a (UnSet xs i) = elem a xs

-- Orden (1)
sizeS :: Eq a => Set a -> Int
sizeS (UnSet xs i) = i

-- Orden (n)
removeS :: Eq a => a -> Set a -> Set a
removeS a (UnSet xs i) = (UnSet (remover a xs) (i - (if elem a xs then 1 else 0)))

remover :: Eq a => a -> [a] -> [a]
remover a [] = []
remover a [x] = if a == x 
    then [] else [x]
remover a (x:xs) = remover a [x] ++ remover a xs


-- Orden (n²)
unionS :: Eq a => Set a -> Set a -> Set a
unionS (UnSet xs i) (UnSet ys j) = (UnSet (union xs ys) (size (union xs ys)))

union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys 
union (x:xs) ys = if not (elem x ys) then x : ys else ys  

size :: [a] -> Int
size [] = 0
size (x:xs) = 1 + size xs

-- Orden (n)
interseccionS :: Eq a => Set a -> Set a -> Set a
interseccionS (UnSet xs i) (UnSet ys j) = (UnSet (interseccion xs ys) (size (interseccion xs ys)))

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] ys = ys 
interseccion (x:xs) ys = if (elem x ys) then ys else x:ys

--Orden (1)
setToList :: Eq a => Set a -> [a]
setToList (UnSet xs i) = xs

