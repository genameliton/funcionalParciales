import Text.Show.Functions

---- PUNTO 1 ----

data Turista = UnTurista{
    cansancio :: Int,
    stress :: Int,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
} deriving (Show)

type Idioma = String

ana :: Turista
ana = UnTurista 0 21 False ["español"]

beto :: Turista
beto = UnTurista 15 15 True ["alemán"]

cathi :: Turista
cathi = UnTurista 15 15 True ["alemán", "catalán"]

---- PUNTO 2 ----

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya turista
    | viajaSolo turista = modificarCansancio (-5) turista
    | otherwise         = modificarStress (-1) turista

modificarStress :: Int -> Turista -> Turista
modificarStress valor turista = turista {stress = stress turista + valor}

modificarCansancio :: Int -> Turista -> Turista
modificarCansancio valor turista = turista {cansancio = cansancio turista + valor}


apreciarAlgunElementoDelPaisaje :: String -> Excursion
apreciarAlgunElementoDelPaisaje elemento = modificarStress (length elemento)

salirAHablarUnIdioma :: Idioma -> Excursion
salirAHablarUnIdioma idioma turista = turista {idiomas = idioma : idiomas turista, viajaSolo = False}

caminar :: Int -> Excursion
caminar valor = modificarCansancio intensidad . modificarStress (-intensidad)
    where intensidad = div valor 4


paseoEnBarco :: String -> Excursion
paseoEnBarco estadoMarea turista
    | estadoMarea == "Fuerte"    = modificarStress 6 . modificarCansancio 10 $ turista
    | estadoMarea == "Moderada"  = turista
    | estadoMarea == "Tranquila" = caminar 10 . apreciarAlgunElementoDelPaisaje "mar" . salirAHablarUnIdioma "alemán" $ turista

-- a --

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = modificarStress (-valorDeStress) . excursion $ turista
    where valorDeStress = applyPercentage (stress . excursion $ turista) 10

applyPercentage :: Int -> Int -> Int
applyPercentage value percentage = (value * percentage) `div` 100

-- b --

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = Turista -> Int

deltaExcursionSegun :: Indice -> Turista -> Excursion -> Int
deltaExcursionSegun indice turista excursion = indice (hacerExcursion excursion turista) - indice turista

-- c --

esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = deltaExcursionSegun (length.idiomas) turista excursion /= 0

excursionesDesestresantes :: [Excursion] -> Turista -> [Excursion]
excursionesDesestresantes excursiones turista = filter (esDesestresante turista) excursiones

esDesestresante:: Turista -> Excursion -> Bool
esDesestresante turista excursion = negate (deltaExcursionSegun stress turista excursion) >= 3

---- PUNTO 3 ----

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarAlgunElementoDelPaisaje "cascada", caminar 40, irALaPlaya, salirAHablarUnIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco "Tranquila", excursion, caminar 120]

islaVecina :: String -> Tour
islaVecina estadoMarea
    | estadoMarea == "Fuerte" = [paseoEnBarco "Fuerte", apreciarAlgunElementoDelPaisaje "lago", paseoEnBarco "Fuerte"]
    | otherwise               = [paseoEnBarco estadoMarea, irALaPlaya, paseoEnBarco estadoMarea]

-- a --

hacerTour :: Tour -> Turista -> Turista
hacerTour tour turista = foldl (flip hacerExcursion) turistaEstresado tour
    where turistaEstresado = modificarStress (length tour) turista

-- b --

esConvincenteTours :: [Tour] -> Turista -> Bool
esConvincenteTours tours turista = any (esConvincente turista) tours

esConvincente :: Turista -> Tour -> Bool
esConvincente turista  = any (loDejaAcompañado turista) . flip excursionesDesestresantes turista

loDejaAcompañado :: Turista -> Excursion -> Bool
loDejaAcompañado turista = not . viajaSolo . flip hacerExcursion turista

-- c --

efectividadDeTour :: Tour -> [Turista] -> Int
efectividadDeTour tour = sum . map (espiritualidadDeTurista tour) . filter (`esConvincente` tour)

espiritualidadDeTurista :: Tour -> Turista -> Int
espiritualidadDeTurista tour turista = negate (deltaSegun sumaDeCansanciosYStress (hacerTour tour turista) turista)

sumaDeCansanciosYStress :: Turista -> Int
sumaDeCansanciosYStress turista = cansancio turista + stress turista

---- PUNTO 4 ----

-- a --

tourInfinito :: Tour
tourInfinito = repeat irALaPlaya

{-
b- Cuando evaluamos con Ana funciona, debido que Haskell apenas encuentra una función que deja acompañada a Ana (que ya lo está) ya devuelve True, sin necesidad de evaluar el resto de las expediciones (lazy evaluation). Mientras que con Beto se queda checkeando para siempre, ya que como está solo e ir a la playa no te deja acompañado, nunca va a encontrar una expedición que sea convincente.

c- No, solo en el caso que la lista de turistas sea 0
-}