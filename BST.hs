-- PRACTICA 5
-- BST

module BST (
    perteneceBST,
    lookupBST,
    insertBST,
    minBST,
    deleteMinBST,
    maxBST,
    deleteMAXBST,
    deleteBST,
    splitMaxBST,
    splitMinBST,
    esBST,
    elMaximoMenorA,
    elMinimoMayorA
)   where

data Tree a = NodeT a (Tree a) (Tree a) | EmptyT

-- funciones

perteneceBST :: Ord a => a -> Tree a -> Bool
perteneceBST x EmptyT = False
perteneceBST x (NodeT y ti td) = if not x == y
    then perteneceTree x ti || perteneceBST x td
    else True 