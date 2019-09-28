data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

generarRaiz :: Int -> Tree Int
generarRaiz i = (NodeT i EmptyT EmptyT) 
generarArbol1Rama :: Int -> Int -> Tree Int
generarArbol1Rama x y = (NodeT x (NodeT y EmptyT EmptyT) EmptyT)
generarArbol2Rama :: Int -> Int -> Int -> Tree Int
generarArbol2Rama x y z = (NodeT x (NodeT y EmptyT EmptyT) (NodeT z EmptyT EmptyT)) 

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x ti td) = [x] : (join (listPerLevel ti) (listPerLevel td))
join :: [[a]] -> [[a]] -> [[a]]
join x [] = x
join [] x = x 
join (x:xs) (y:ys) = (x++y) : join xs ys

--todosLosCaminos :: Tree a -> [[a]]
--todosLosCaminos EmptyT = ...
--todosLosCaminos (NodeT x EmptyT EmptyT) = 
--todosLosCaminos (NodeT x ti td) = 
--1
--1
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x ti td) = x + (sumarT ti) + (sumarT td)
--2
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x ti td) = 1 + (sizeT ti) + (sizeT td)
--3
mapDobleT :: Num a => Tree a -> Tree a
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x ti td) = NodeT (x*2) (mapDobleT ti) (mapDobleT td)
--4
mapLongitudT :: Tree String -> Tree Int
mapLongitudT EmptyT = EmptyT
mapLongitudT (NodeT x ti td) = NodeT (length x) (mapLongitudT ti) (mapLongitudT td)
--5
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT y (NodeT x ti td) = (x == y) || (perteneceT x ti) || (perteneceT x td)
--6
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT y (NodeT x ti td) = (if (x == y) then 1 else 0) + aparicionesT x ti + aparicionesT x td
--7
countLeaves :: Tree a -> Int
countLeaves EmptyT = 0
countLeaves (NodeT x EmptyT EmptyT) = 1
countLeaves (NodeT x ti td) = countLeaves ti + countLeaves td
--8
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x ti td) = leaves ti ++ leaves td
--9
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x EmptyT EmptyT) = 1
heightT (NodeT x ti td) = 1 + heightT ti + heightT td
--10
countNotLeaves :: Tree a -> Int
countNotLeaves EmptyT = 0
countNotLeaves (NodeT x EmptyT EmptyT) = 0
countNotLeaves (NodeT x ti td) = 1 + countNotLeaves ti + countNotLeaves td

--11
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x td ti) = NodeT x ti td
--12
listInOrder :: Tree a -> [a]
listInOrder EmptyT =  []
listInOrder (NodeT x EmptyT EmptyT) = [x]
listInOrder (NodeT x ti td) = (listInOrder ti) ++ (x : (listInOrder td))
--13
listPreOrder :: Tree a -> [a]
listPreOrder EmptyT = []
listPreOrder (NodeT x EmptyT EmptyT) = [x]
listPreOrder (NodeT x ti td) = (x : listPreOrder ti) ++ listPreOrder td
--14
listPosOrder :: Tree a -> [a]
listPosOrder EmptyT = []
listPosOrder (NodeT x EmptyT EmptyT) = [x]
listPosOrder (NodeT x ti td) = (listPosOrder ti) ++ (listPosOrder td ++ [x])
--15
concatenarListasT :: Tree [a] -> [a]
concatenarListasT EmptyT = []
concatenarListasT (NodeT x EmptyT EmptyT) = x
concatenarListasT (NodeT x ti td) = (concatenarListasT ti) ++ x ++ (concatenarListasT td)
--16
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = error "Lista esta vacia"
levelN 0 (NodeT x ti td) = [x] 
levelN n (NodeT x ti td) = (levelN (n-1) ti) ++ (levelN (n-1) td) 
--17
--arriba
--18
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x EmptyT EmptyT) = [x]
ramaMasLarga (NodeT x ti td) = [x] ++ (if (sizeT ti) > (sizeT td) then (ramaMasLarga ti) else (ramaMasLarga td))
--19
join2 :: a -> [[a]] -> [[a]]
join2 _ [] = []
join2 x (y:ys) = (x:y) : (join2 x ys)

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x ti td) = join2 x (todosLosCaminos ti ++ todosLosCaminos td) 
--20
altura :: Tree a -> Int
altura EmptyT = 0
altura (NodeT x EmptyT EmptyT) = 0
altura (NodeT x ti td) = 1 + altura ti + altura td 

balanceado :: Tree a -> Bool
balanceado EmptyT = False
balanceado (NodeT x EmptyT EmptyT) = True
balanceado (NodeT x ti EmptyT) = False
balanceado (NodeT x EmptyT td) = False
balanceado (NodeT x ti td) = ((altura ti) - (altura td)) <= 0
 
-- TO BE CONTINUED...

--2
data Dir = Izq | Der deriving (Show, Eq) -- agregar Inicio?
data Objeto = Tesoro | Chatarra deriving (Show, Eq)
data Mapa = Cofre Objeto | Bifurcacion Objeto Mapa Mapa deriving Show

--generadores

generarCofreObj :: Objeto -> Mapa
generarCofreObj obj = (Cofre obj)

generarBif3Obj :: Objeto -> Objeto -> Objeto -> Mapa
generarBif3Obj obj1 obj2 obj3 = (Bifurcacion obj1 (Cofre obj2) (Cofre obj3))

mapaTest = Bifurcacion Chatarra (Bifurcacion Chatarra (Cofre Tesoro) (Cofre Chatarra)) (Cofre Chatarra)
--aux para p1
esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False 
--1
hayTesoro :: Mapa -> Bool
hayTesoro (Cofre obj) = esTesoro obj
hayTesoro (Bifurcacion obj map1 map2) = esTesoro obj || hayTesoro map1 || hayTesoro map2

--aux para p2
hayTesoroAca :: Mapa -> Bool
hayTesoroAca (Cofre obj) = (esTesoro obj)
hayTesoroAca (Bifurcacion obj _ _) = (esTesoro obj)


hayTesoroEnDireccion :: Dir -> Mapa -> Bool
hayTesoroEnDireccion dir (Cofre obj) = (esTesoro obj) 
--hayTesoroEnDireccion dir (Bifurcacion obj map1 )
hayTesoroEnDireccion dir (Bifurcacion obj map1 map2) =
    if (dir == Der) then 
        hayTesoroEnDireccion dir map2
        else
            hayTesoroEnDireccion dir map1
    

--2
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] mapa = hayTesoroAca mapa
    {--where
        hayTesoroAca (Cofre obj) = esTesoro obj
        hayTesoroAca (Bifurcacion obj _ _) = (esTesoro obj)--}
--hayTesoroEn d _ = False
hayTesoroEn (Izq : ds) (Bifurcacion _ ti td) = hayTesoroEn ds ti
hayTesoroEn (Der : ds) (Bifurcacion _ ti td) = hayTesoroEn ds td
--3
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Cofre Tesoro) = []
caminoAlTesoro (Cofre Chatarra) = error "no hay tesoro en mapa"
caminoAlTesoro (Bifurcacion Tesoro _ _) = []
caminoAlTesoro (Bifurcacion obj ti td) =
    if hayTesoro td then 
        [Der] ++ caminoAlTesoro td
    else
        [Izq] ++ caminoAlTesoro ti
--4
sizeC :: Mapa -> Int
sizeC (Cofre _) = 1
sizeC (Bifurcacion obj ti td) = 1 + sizeC ti + sizeC td

caminoMasLargo :: Mapa -> [Dir]
caminoMasLargo (Cofre _) = []
caminoMasLargo (Bifurcacion obj ti td) =  
    (if (sizeC ti > sizeC td) then Izq else Der) : [] ++ (caminoMasLargo ti) ++ (caminoMasLargo td)
--5
tesoroPerLevel :: Mapa -> [[Objeto]]
tesoroPerLevel (Cofre obj) = if (esTesoro obj) then [[obj]] else []
tesoroPerLevel (Bifurcacion obj ti td) = (if (esTesoro obj) then [[obj]] else []) ++ (tesoroPerLevel ti ++ tesoroPerLevel td)
--6
temp :: Mapa -> Bool
temp (Cofre _) = True
temp _ = False

join3 :: Dir -> [[Dir]] -> [[Dir]]
join3 _ [] = []
join3 dir (ds:dss) = (dir : ds) : (join3 dir dss) 

todosLosCaminosT :: Mapa -> [[Dir]]
todosLosCaminosT (Cofre _) = []
todosLosCaminosT (Bifurcacion obj (Cofre _) (Cofre _)) = [[Izq], [Der]]
todosLosCaminosT (Bifurcacion obj ti (Cofre _)) = join3 Izq (todosLosCaminosT ti) ++ [[Der]]
todosLosCaminosT (Bifurcacion obj (Cofre _) td) = [[Izq]] ++ join3 Der (todosLosCaminosT td) 
todosLosCaminosT (Bifurcacion obj ti td) =  join3 Izq (todosLosCaminosT ti) ++ join3 Der (todosLosCaminosT td)


mapa = Bifurcacion Tesoro 
            (Bifurcacion Tesoro (Cofre Chatarra) (Cofre Chatarra)) 
            (Bifurcacion Tesoro 
                (Bifurcacion  Chatarra (Cofre Chatarra) (Cofre Chatarra)) 
                (Cofre Tesoro))

--4

data Exp = Constante Int | ExpUnaria OpUnaria Exp | ExpBinaria OpBinaria Exp Exp deriving Show
data OpUnaria = Neg deriving Show
data OpBinaria = Suma | Resta | Mult | Div deriving Show

--1
eval :: Exp -> Int
eval (Constante i) = i 
eval (ExpUnaria Neg x) = (eval x) * (-1)
eval (ExpBinaria Suma x y) = (eval x) + (eval y)
eval (ExpBinaria Resta x y) = (eval x) - (eval y)
eval (ExpBinaria Mult x y) = (eval x) * (eval y)
eval (ExpBinaria Div x y) = div (eval x) (eval y)

--2
simplificar :: Exp -> Exp
simplificar (ExpBinaria Suma (Constante 0) (Constante n)) = Constante n

