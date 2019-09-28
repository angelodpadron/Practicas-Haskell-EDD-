-- TP 1

-- E1
-- a
sucesor :: Int -> Int
sucesor x = x+1
--b
sumar :: Int -> Int -> Int
sumar x y = x + y
--c
maximo :: Int -> Int -> Int
maximo x y =
  if x > y
    then x
    else y

--E2
--a
-- estandar
negar :: Bool -> Bool
negar x = not x
-- con pattern matching
negar' :: Bool -> Bool
negar' n 
  | n == True = False
  | n == False = True
-- pattern matching 
negar'' x =
  if x /= True
    then True
    else False
--b
andLogico :: Bool -> Bool -> Bool
andLogico False False = False
andLogico True True = True
andLogico False True = False
andLogico True False = False
--c
orLogico :: Bool -> Bool -> Bool
orLogico True False = True
orLogico False True = True
orLogico True True = True
orLogico False False = False
--d
primera :: (x,y) -> x
primera (x,y) = x
--e
segunda :: (x,y) -> y
segunda (x,y) = y
--f
-- estandar
sumaPar :: (Int,Int) -> Int
sumaPar (x,y) = x+y
--g
-- maximoDelPar :: (x,y) -> x -- ponele
maximoDelPar (x,y)
  | x > y = x

  | y > x = y

--E3
--a
loMismo :: x -> x
loMismo x = x
--b
siempreSiete :: x -> Int
siempreSiete x = 7
--c
duplicar :: x -> (x,x)
duplicar x = (x,x)
--d
singleton :: a -> [a]
singleton a = a: []

--E4
--a
--isEmpty' :: [a] -> Bool
--isEmpty' [] = True
--isEmpty' (x:xs) = (x:xs) == [] 
--b
head' :: [a] -> a
head' (a:_) = a
--c 
tail' :: [a] -> [a]
tail' [] = []
tail' (a:ab) = ab

--Recursion
--1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria [x] = x
sumatoria (x:xs) = head' (x:xs) + sumatoria (tail'(x:xs))
--2
-- lean's version
longitud' :: [a] -> Int
longitud' [] = 0
longitud' (x:xs) = 1 + (longitud xs)


longitud :: [a] -> Int
longitud [] = 0
longitud [x] = 1
longitud (x:xs) = longitud [x] + longitud (tail'(x:xs))
--3
mapSucesor :: [Int] -> [Int]
mapSucesor [] = []
mapSucesor [x] = [x+1]
mapSucesor (x:xs) = sucesor x : mapSucesor (tail'(x:xs))
--4
mapSumaPar :: [(Int,Int)] -> [Int]
mapSumaPar [] = []
mapSumaPar [(x,y)] = [x+y]
mapSumaPar (x:xs) = sumaPar x : mapSumaPar xs
--5
mapMaxDelPar :: [(Int, Int)] -> [Int]
mapMaxDelPar [] = []
mapMaxDelPar [(x,y)]
  | x > y = [x]
  | y > x = [y]
  | x == y = [x] 
mapMaxDelPar (x:xs) = mapMaxDelPar [x] ++ mapMaxDelPar xs
--6
todoVerdad :: [Bool] -> Bool
todoVerdad [] = False
todoVerdad [x] = andLogico x True
todoVerdad (x:xs) = andLogico (todoVerdad [x]) (todoVerdad xs)
--7
algunaVerdad :: [Bool] -> Bool
algunaVerdad [] = False
algunaVerdad [x] = todoVerdad [x]
algunaVerdad (x:xs) = orLogico (algunaVerdad [x]) (algunaVerdad xs)
--8
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x [y] = x == y 
pertenece x (y:ys) = orLogico (pertenece x [y]) (pertenece x ys)
--9
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones x [y]
  | x == y = 1
apariciones x (y:ys) = apariciones x [y] + apariciones x ys
--10
filtrarMenoresA :: Int -> [Int] -> [Int]
filtrarMenoresA _ [] = []
filtrarMenores x [y]
  |x == y = []     -- no borrar, overflow
  |x < y = []
  |y < x = [y]
filtrarMenores x (y:ys) = filtrarMenores x [y] ++ filtrarMenores x ys
--11
filtrarElemento :: Eq a => a -> [a] -> [a]
filtrarElemento _ [] = []
filtrarElemento x [y] 
  |x == y = []
  |x /= y = [y]
filtrarElemento x (y:ys) = filtrarElemento x [y] ++ filtrarElemento x ys
--12
mapLongitudes :: [[a]] -> [Int]
mapLongitudes [] = []
mapLongitudes (x:xs) = longitud x  : mapLongitudes xs
--13
longitudMayorA :: Int -> [[a]] -> [[a]]
longitudMayorA _ [] = []
longitudMayorA i (x:xs)
  |longitud x < i = xs
  |longitud x >= i = x : longitudMayorA i xs
--14

intercalar :: a -> [a] -> [a]
intercalar i [] = []
intercalar i (x:xs) = [x, i] ++ intercalar i xs 

--15
--solo pattern matching
snoc ::  [a] -> a -> [a]
snoc [] y = [y]
snoc (x:xs) y = (x:xs) ++ [y]
--16
append :: [a] -> [a] -> [a]
append x [] = x
append [] x = x
append (x:xs) (y:ys) = [x,y] ++ append xs ys
--17
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs
--18
reversa :: [a] -> [a]
reversa [] = []
reversa [x] = [x]
reversa (x:xs) = snoc (reversa xs) x
--19
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = error "listas vacias"
zipMaximos [] x = x
zipMaximos x [] = x
zipMaximos (x:xs) (y:ys) = maximo x y : zipMaximos xs ys
--20
zipSort :: [Int] -> [Int] -> [(Int, Int)]
zipSort [] [] = []
zipSort x y = [((minimum x), (maximum y))]
--21
--sin recursion
promedio :: [Int] -> Int
promedio [] = error "lista vacia"
promedio x = div (sumatoria x) (longitud x)
--22
minimo :: Ord a => [a] -> a 
minimo [] = error "lista vacia"
minimo [x] = x  
minimo (x:y:xs) = if x < y 
  then minimo(x:xs) 
  else minimo(y:xs)
-- 2.2
--1
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)
--2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = [n] ++ cuentaRegresiva (n-1)
--3
--sin recursion
contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = reversa (cuentaRegresiva n)
--5
zipSort' :: [Int] -> [Int] -> [(Int, Int)]
zipSort' [] [] = []
zipSort' (x:xs) [] = [(x,0)]
zipSort' [] (y:ys) = [(y,0)]
zipSort' (x:xs) (y:ys) =
  if x<y
    then (x,y) : zipSort' xs ys
    else (y,x) : zipSort' xs ys


