-- PRACTICA 5

import Map

buscarClaves :: Eq k => [k] -> Map k v -> [Maybe v]
buscarClaves [] _ = []
buscarClaves (k:ks) mapa = (lookUpM k mapa) : buscarClaves ks mapa

estanTodas :: Eq k => [k] -> Map k v -> Bool
estanTodas ks mapa = estanTodas' ks (domM mapa)

estanTodas' :: Eq k => [k] -> [k] -> Bool
estanTodas' [] _ = True
estanTodas' (k:ks) lista = (elem k lista) && estanTodas' ks lista

actualizarClaves :: Eq k => [(k,v)] -> Map k v -> Map k v
actualizarClaves [] mapa = mapa 
actualizarClaves (kv:kvs) mapa = assocM (fst kv) (snd kv) (actualizarClaves kvs mapa)

unirDoms :: Eq k => [Map k v] -> [k]
unirDoms [] = []
unirDoms (m:ms) = union (domM m) (unirDoms ms)

union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys 
union (x:xs) ys = union xs (if (elem x ys) 
    then ys 
    else x : ys)

--mapSuccM :: Eq k => [k] -> Map k Int -> Map k Int
