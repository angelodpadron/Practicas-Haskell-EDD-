-- PRACTICA 4

module Queue_v2 (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue) where

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
queue e (Q xs) = (Q (e:xs))         -- agrega por delante

-- Orden (1)
firstQ :: Queue a -> a
firstQ (Q (xs)) = last xs

-- Orden (1)
dequeue :: Queue a -> Queue a
dequeue (Q (xs)) = (Q (tail xs))    
