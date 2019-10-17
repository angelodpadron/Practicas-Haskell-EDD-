-- PRACTICA 5

module Map_v3 (Map, emptyM, assocM, lookUpM, deleteM, domM) where

data Map k v = M [k] [v] deriving (Show, Eq)
--data Maybe e = Just e | Nothing

-- Orden (1)
emptyM :: Map k v
emptyM = M [] []

-- Orden (1)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM key val (M ks vs) = M (ks : [key]) (vs : [val])  

lookUpM :: Eq k => k -> Map k v -> Maybe v
lookUpM key (M ks vs) = lookUp key ks vs


--aux (Orden (n²))
lookUp :: Eq k => k -> [k] -> [v] -> Maybe v
lookUp key ks [] = Nothing 
lookUp key ks vs = getValueAt ((getPosOf key ks) vs)

getPosOf :: Eq k => k -> [k] -> Int
getPosOf key [] = 0
getPosOf key (k:ks) = (if key == k then 0 else 1) + (getValueAt key ks)

getValueAt :: Int -> [v] -> Maybe v
getValueAt i [] = Nothing
getValueAt i (v:vs) = if i > 0 then getValueAt vs else Just v


-- Orden (n²)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM key (M ks ys) =  (M (borrarKV key xs))

borrarKV :: Eq k => k -> [(k,v)] -> [(k,v)]
borrarKV key [] = []
borrarKV key (x:xs) = if (key == (fst x)) 
    then xs
    else x : (borrarKV key xs)

domM :: Map k v -> [k]
domM (M xs) = domLite xs 

domLite :: [(k, v)] -> [k]
domLite [] = []
domLite (x:xs) = (fst x) : domLite xs