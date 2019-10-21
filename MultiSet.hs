-- PRACTICA 5

module MultiSet(
    emptyMS,
    addMS,
    ocurrencesMS,
    unionMS,
    intersectionMS,
    --multiSetToList
) where

data MultiSet a = MS [a] Int deriving (Show)

--funciones

-- Orden (1)
emptyMS :: MultiSet a
emptyMS = MS [] 0

--Orden (n)
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS e (MS xs i) = (MS (e:xs) (i+1)) 

--Orden (1 + n²)
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS e (MS xs i) = ocurrencesL e xs

--Orden (n²)
ocurrencesL :: Eq a => a -> [a] -> Int
ocurrencesL _ [] = 0
ocurrencesL e (x:xs) = if (e == x) 
    then 1 + (ocurrencesL e xs)
    else 0 + (ocurrencesL e xs) 

--Orden (n²)
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MS xs i) (MS ys j) = (MS (unionL xs ys) (length (unionL xs ys)))

unionL :: Eq a => [a] -> [a] -> [a]
unionL [] ys = ys 
unionL (x:xs) ys = unionL xs (if (elem x ys) then ys else x : ys)

--Orden (n²)
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersectionMS (MS xs i) (MS ys j) = (MS (intersectionL xs ys) (length (intersectionL xs ys)))

intersectionL :: Eq a => [a] -> [a] -> [a]
intersectionL [] ys = ys 
intersectionL (x:xs) ys = intersectionL xs (if (elem x ys) then ys else (x:ys)) 


multiSetToList :: Eq a => MultiSet a -> [(Int, a)]
multiSetToList (MS xs i) = listToList xs

listToList :: Eq a => [a] -> [(Int, a)]
listToList [] = []
listToList (x:xs) = ((ocurrencesL x (x:xs)), x) : (listToList xs) 