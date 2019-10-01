-- PRACTICA 4 Modulo

--3
module Queue (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue) where

data Queue a = Q [a] deriving (Show, Eq)

-- Orden (1)
emptyQ :: Queue a
emptyQ = (Q [])

-- Orden (?)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = (size xs) == 0

size :: [a] -> Int
size [] = 0
size (x:xs) = 1 + size xs

-- Orden (1)
queue :: a -> Queue a -> Queue a
queue e (Q xs) = (Q (xs ++ [e])) 

-- Orden (1)
firstQ :: Queue a -> a
firstQ (Q xs) = last xs

-- Orden (1)
dequeue :: Queue a -> Queue a
dequeue (Q xs) = (Q (init xs))    