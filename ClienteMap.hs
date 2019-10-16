-- PRACTICA 5

-- Ejercicio 1

--import Map version_1
import Map_v2


--e1
buscarClaves :: Eq k => [k] -> Map k v -> [Maybe v]
buscarClaves [] _ = []
buscarClaves (k:ks) mapa = (lookUpM k mapa) : buscarClaves ks mapa

--e2
estanTodas :: Eq k => [k] -> Map k v -> Bool
estanTodas ks mapa = estanTodas' ks (domM mapa)

estanTodas' :: Eq k => [k] -> [k] -> Bool
estanTodas' [] _ = True
estanTodas' (k:ks) lista = (elem k lista) && estanTodas' ks lista

--e3
actualizarClaves :: Eq k => [(k,v)] -> Map k v -> Map k v
actualizarClaves [] mapa = mapa 
actualizarClaves (kv:kvs) mapa = assocM (fst kv) (snd kv) (actualizarClaves kvs mapa)

--e4
unirDoms :: Eq k => [Map k v] -> [k]
unirDoms [] = []
unirDoms (m:ms) = union (domM m) (unirDoms ms)

union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys 
union (x:xs) ys = union xs (if (elem x ys) 
    then ys 
    else x : ys)

--e5
-- Orden (n²)
mapSuccM :: Eq k => [k] -> Map k Int -> Map k Int
mapSuccM [] mapa = mapa 
mapSuccM (k:ks) mapa = assocM k (unZip (lookUpM k mapa) + 1) (mapSuccM ks mapa) 

-- Orden (1)
unZip :: Maybe a -> a
unZip (Nothing) = error "no existe valor asociado"
unZip (Just x) = x

--e6
agregarMap :: Eq k => Map k v -> Map k v -> Map k v
agregarMap mapa1 mapa2 = assocEmAll (listaDatos mapa1) mapa2

-- Orden (n)
assocEmAll :: Eq k => [(k, v)] -> Map k v -> Map k v
assocEmAll [] mapa = mapa
assocEmAll (kv:kvs) mapa = assocM (fst kv) (snd kv) (assocEmAll kvs mapa)

-- Orden (n²)
listaDatos :: Eq k => Map k v -> [(k, v)]
listaDatos mapa = zip (domM mapa) (imgM (domM mapa) mapa)

--Orden (n)
imgM :: Eq k => [k] -> Map k v -> [v]
imgM [] mapa = []
imgM (k:ks) mapa = (unZip (lookUpM k mapa)) : imgM ks mapa --para evitar error de Nothing, deberia existir subrutina que devuelva [] al ser dicho caso

