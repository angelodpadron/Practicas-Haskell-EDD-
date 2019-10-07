-- PRACTICA 4

module Stack (Stack, emptyS, isEmptyS, push, top, pop) where

data Stack a = S [a] deriving (Show, Eq)

-- Orden (1)
emptyS :: Stack a
emptyS = (S [])

-- Orden (n)
isEmptyS :: Stack a -> Bool
isEmptyS (S xs) = size xs == 0

size :: [a] -> Int
size [] = 0
size (x:xs) = 1 + size xs

-- Orden (1)
push :: a -> Stack a -> Stack a
push e (S xs) = (S (xs ++ [e]))

top :: Stack a -> a
top (S xs) = last xs

pop :: Stack a -> Stack a
pop (S xs) = (S (tail xs))