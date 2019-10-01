--PRACTICA 4

import Queue

lenghtQ :: Queue a -> Int
lenghtQ x = 
    if (isEmptyQ x) 
        then 0 
        else 1 + (lenghtQ (dequeue x)) 

queueToList:: Queue a -> [a]
queueToList x = 
    if isEmptyQ x 
        then []
        else
            [firstQ x] ++ queueToList (dequeue x)

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = 
    if isEmptyQ q1
        then q2
        else unionQ (queue (firstQ q2) q1) (dequeue q2) -- revisar!

union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys 
union (x:xs) ys = union xs (if (elem x ys) then ys else x : ys)