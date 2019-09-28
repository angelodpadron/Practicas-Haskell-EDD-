--1
data Dir = Norte | Sur | Este | Oeste deriving Show
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte
--2
data Persona = P String Int deriving Show
nombre :: Persona -> String
nombre (P nombre edad) = nombre                         
edad :: Persona -> Int
edad (P nombre edad) = edad 
crecer :: Persona -> Persona 
crecer (P nombre edad) = P nombre (edad+1)
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nNombre (P nombre edad) = P nNombre edad
esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra (P n1 e1) (P n2 e2) = e1 < e2
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA i (x:xs) 
    | edad x >= i = x : mayoresA i xs
    | edad x < i = mayoresA i xs

--auxiliar
sumaEdades :: [Persona] -> Int
sumaEdades [] = 0
sumaEdades [x] = edad x
sumaEdades (x:xs) = sumaEdades [x] + sumaEdades xs 
promedioEdad :: [Persona] -> Int
promedioEdad [x] = edad x
promedioEdad x = div (sumaEdades x) (length x)

--auxiliar
elMasViejoDe2 :: Persona -> Persona -> Persona 
elMasViejoDe2 _ x = x
elMasViejoDe2 x _ = x
elMasViejoDe2 p1 p2 
    | (edad p1) > (edad p2) = p1
    | (edad p1) <= (edad p2) = p2

elMasViejo ::  [Persona] -> Persona
elMasViejo [x] = x
elMasViejo (x:xs) = elMasViejoDe2 x (elMasViejo xs)

--3 (no me denuncies, Nintendo)
data Pokemon = PK String TipoDePokemon Energia deriving Show
data TipoDePokemon = Agua | Fuego | Planta deriving Show
type Energia = Int
data Entrenador = TR String [Pokemon] deriving Show

--constantes
pepe = (TR "Pepe" [baulbasaur, charmander, squirtle])
baulbasaur = (PK "Baulbasaur" Planta 80)  
charmander = (PK "Charmander" Fuego 90)
squirtle = (PK "Squirtle" Agua 83)

esMasPolenta :: TipoDePokemon -> TipoDePokemon -> Bool
esMasPolenta Agua Fuego = True 
esMasPolenta Fuego Planta = True
esMasPolenta Planta Agua = True
esMasPolenta _ _ = False
pokedex :: Pokemon -> TipoDePokemon
pokedex (PK nombre tipo energia) = tipo
sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Agua Agua = True
sonMismoTipo Fuego Fuego = True
sonMismoTipo Planta Planta = True
sonMismoTipo _ _ = False
contieneDeTipo :: TipoDePokemon -> [Pokemon] -> Bool
contieneDeTipo _ [] = False
contieneDeTipo tipo (pk:pks) = sonMismoTipo tipo (pokedex pk) || contieneDeTipo tipo pks  

leGanaAlToque ::  Pokemon -> Pokemon -> Bool 
leGanaAlToque pk1 pk2 = esMasPolenta (pokedex pk1) (pokedex pk2)
capturarPokemon :: Pokemon -> Entrenador -> Entrenador
capturarPokemon pokemon (TR nombre [pokemones]) = TR nombre (pokemon : [pokemones])
cantidadDePokemones :: Entrenador -> Int
cantidadDePokemones (TR nombre [pokemones]) = length [pokemones]
cantidadDePokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> Int
cantidadDePokemonesDeTipo _ [] = 0 
cantidadDePokemonesDeTipo tipo (pk:pks) = 
    (if (sonMismoTipo (pokedex pk) tipo) 
        then 1 
        else 0) 
        + cantidadDePokemonesDeTipo tipo pks   
esExperto :: Entrenador -> Bool
esExperto (TR nombre []) = False 
esExperto (TR nombre [pokemon]) = 
    (contieneDeTipo Agua [pokemon]) && 
    (contieneDeTipo Fuego [pokemon]) && 
    (contieneDeTipo Planta [pokemon])

--4
data Pizza = Prepizza | Agregar Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Jamon | Queso | AceitunasVerdes Int deriving (Show, Eq)

sonMismoIngrediente :: Ingrediente -> Ingrediente -> Bool
sonMismoIngrediente (AceitunasVerdes _) (AceitunasVerdes _) = True 
sonMismoIngrediente i1 i2 = i1 == i2 

cantidadDeAceitunas :: Ingrediente -> Int
cantidadDeAceitunas (AceitunasVerdes x) = x

tieneIngrediente :: Ingrediente -> Pizza -> Bool
tieneIngrediente _ Prepizza = False 
tieneIngrediente ingrediente (Agregar ing pizza) = sonMismoIngrediente ingrediente ing || tieneIngrediente ingrediente pizza   

sacarIngrediente :: Ingrediente -> Pizza -> Pizza
sacarIngrediente _ Prepizza = Prepizza
sacarIngrediente ing (Agregar ingrediente pizza) =
    if (sonMismoIngrediente ing ingrediente)
        then sacarIngrediente ing pizza 
        else Agregar ingrediente (sacarIngrediente ing pizza)

pizzasQueContienen :: Ingrediente -> [Pizza] -> [Pizza]
pizzasQueContienen _ [] = []
pizzasQueContienen _ [Prepizza] = []
pizzasQueContienen ingrediente (p:pz) = if tieneIngrediente ingrediente p 
    then p : (pizzasQueContienen ingrediente pz)
    else (pizzasQueContienen ingrediente pz)

cantIngrediente :: Ingrediente -> Pizza -> Int
cantIngrediente _ Prepizza = 0
cantIngrediente ing (Agregar ingrediente pizza) = (if (ingrediente == ing) then 1 else 0) + (cantIngrediente ing pizza) 

ingredientes :: Pizza -> [Ingrediente]
ingredientes Prepizza = []
ingredientes (Agregar ingrediente pizza) = ingrediente : ingredientes pizza

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:is) = (Agregar i (armarPizza is))

esAceituna :: Ingrediente -> Bool
esAceituna ing = sonMismoIngrediente ing (AceitunasVerdes 0) 

duplicarAceitunasAux :: Ingrediente -> Ingrediente
duplicarAceitunasAux (AceitunasVerdes cant) = (AceitunasVerdes (cant * 2)) 

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza 
duplicarAceitunas (Agregar ingrediente pizza) = 
    if (esAceituna ingrediente)
        then (Agregar (duplicarAceitunasAux ingrediente) pizza) 
        else duplicarAceitunas pizza

sacar :: [Ingrediente] -> Pizza -> Pizza 
sacar _ Prepizza = Prepizza
sacar [] pizza = pizza
sacar (i:is) pizza = sacar is (sacarIngrediente i pizza)

--cantJamon :: [Pizza] -> [(Int, Pizza)]
--cantJamon [] = []
--cantJamon (p:pz) = (cantIngrediente Jamon, p) : cantJamon pz

data Objeto = Cacharro | Tesoro deriving (Show, Eq)
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving (Show)

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada _)  = False
hayTesoro (Cofre (o:os) camino) =  Tesoro == o || hayTesoro (Cofre os camino)

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro [Tesoro] = True
tieneTesoro (o:os) = o == Tesoro || tieneTesoro os

{--
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro Nada camino = 1
pasosHastaTesoro (Cofre (t:ts) camino) = if t == Tesoro then + pasosHastaTesoro (Cofre ts camino)
--}

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = error "no hay camino"
pasosHastaTesoro (Nada camino) = 1 + (pasosHastaTesoro camino)
pasosHastaTesoro (Cofre os camino) = if tieneTesoro os then 0 else (1 + pasosHastaTesoro camino)

tesoroEnPrimerPosicion :: Camino -> Bool
tesoroEnPrimerPosicion Fin = False 
tesoroEnPrimerPosicion (Nada _) = False
tesoroEnPrimerPosicion (Cofre obj camino) = tieneTesoro obj 

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 camino = tesoroEnPrimerPosicion camino
hayTesoroEn n Fin = False
hayTesoroEn n (Nada camino) = hayTesoroEn (n-1) camino
hayTesoroEn n (Cofre obj camino) = hayTesoroEn (n-1) camino

cantTesorosEnCamino :: Camino -> Int
cantTesorosEnCamino Fin = 0
--cantTesorosEnCamino Nada Fin = 0
--cantTesorosEnCamino Nada camino = 0 + cantTesorosEnCamino camino
cantTesorosEnCamino (Cofre obj camino) =
    (if tieneTesoro obj then 1 else 0) + cantTesorosEnCamino camino


alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 Fin = True
alMenosNTesoros _ Fin = False
alMenosNTesoros n camino = n <= cantTesorosEnCamino camino 


-----------------------------

data ListaNoVacia a = Unit a | Cons a (ListaNoVacia a)

length' :: ListaNoVacia a -> Int
length' Unit _ = 1
lenght' (Cons a lista) = 1 + lenght' lista 