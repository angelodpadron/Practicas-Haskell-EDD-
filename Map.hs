-- PRACTICA 5

module Map (Map, emptyM, assocM, lookUpM, deleteM, domM) where

data Map k v = M [(k,v)] deriving (Show, Eq)
--data Maybe e = Just e | Nothing

-- Orden (1)
emptyM :: Map k v
emptyM = M []

-- Orden (1)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM key val (M xs) = M ((key, val) : xs) 

lookUpM :: Eq k => k -> Map k v -> Maybe v
lookUpM key (M xs) = lookUp key xs

--aux (Orden (n²))
lookUp :: Eq k => k -> [(k,v)] -> Maybe v
lookUp key [] = Nothing
lookUp key (x:xs) = if (key == (fst x)) 
    then Just (snd x)
    else lookUp key xs

-- Orden (n²)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM key (M xs) =  (M (borrarKV key xs))

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