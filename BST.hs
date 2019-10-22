module BST (perteneceBST,
    lookupBST,
    insertBST,
    minBST,
    deleteMinBST,
    maxBST,
    deleteMaXBST,
    deleteBST,
    splitMaxBST,
    splitMinBST,
    esBST,
    elMaximoMenorA,
    elMinimoMayorA
) where

data (Ord a, Eq a) => Tree a = EmptyT | NodeT (Tree a) a (Tree a) deriving Show


-- Orden (log n)
perteneceBST :: Ord a => (Tree a) -> a -> Bool
perteneceBST EmptyT _ = False
perteneceBST (NodeT t1 v t2) x =
    if x == v 
        then True
    else 
        if x < v 
            then perteneceBST t1 x
    else perteneceBST t2 x

--Orden (1 Log n)
lookupBST :: Ord k => k -> Tree (k, v) -> Maybe v
lookupBST x EmptyT = Nothing
lookupBST x (NodeT ti map td) = 
    if (enMap x map) 
        then (Just (snd map))
    else
        if (x < (fst map))
            then lookupBST x ti
        else lookupBST x td

--Orden (1)
enMap :: Eq k => k -> (k, v) -> Bool
enMap x (k, v) = x == k
enMap _ _ = False

--Orden (1 log n)
insertBST :: Ord a => a -> Tree a -> Tree a
insertBST a EmptyT = (NodeT EmptyT a EmptyT)
insertBST a (NodeT ti v td) =
    if (same a v)
        then (NodeT ti a td)
    else
        if (a < v)
            then insertBST a ti
        else
            insertBST a td

--Orden (1)
same :: Eq a => a -> a -> Bool
same x y = x == y

--Orden (log n)
minBST :: Ord a => Tree a -> a
minBST EmptyT _ = error "arbol sin valor"
minBST (NodeT EmptyT v td) = v
minBST (NodeT ti v td) = minBST ti 

--Orden (log n)
deleteMinBST :: Ord a => Tree a -> Tree a
deleteMinBST EmptyT = error "arbol sin valor"
deleteMinBST (NodeT EmptyT v td) = td
deleteMinBST (NodeT ti v td) = deleteMinBST ti

--Orden (log n)
maxBST :: Ord a => Tree a -> a
maxBST EmptyT = error "arbol sin valor"
maxBST (NodeT td v EmptyT) = v
maxBST (NodeT ti v td) = maxBST td

--Orden (log n)
deleteMaXBST :: Ord a => Tree a -> Tree a
deleteMaXBST EmptyT = error "arbol sin valor"
deleteMaXBST (NodeT ti v EmptyT) = ti
deleteMaXBST (NodeT ti v td) = deleteMaXBST td

--Orden (1 log n)
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST _ EmptyT = error "arbol sin valor"
deleteBST a (NodeT ti v td) =
    if same a v
        then (NodeT ti (minBST td) (deleteMinBST td))
    else
        if (a < v)
            then deleteBST a ti
        else
            deleteBST a td

--Orden (log n + log m)
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST EmptyT = error "arbol sin valor"
splitMinBST arbol = ((minBST arbol), (deleteMinBST arbol)) 

--Orden (log n + log m)
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST EmptyT = error "arbol sin valor"
splitMaxBST arbol = ((maxBST arbol), (deleteMaXBST arbol))

esBST :: Ord a => Tree a -> Bool
esBST EmptyT = False
esBST (NodeT ti v td) = 