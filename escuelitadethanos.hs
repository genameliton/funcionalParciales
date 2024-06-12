import Text.Show.Functions
---- PUNTO 1 ----

data Personaje = UnPersonaje {
    edad :: Int,
    energia :: Float,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: String
} deriving (Show)

data Guantelete = UnGuantelete {
    material :: String,
    gemas :: [Gema]
}

type Universo = [Personaje]
type Gema = Personaje -> Personaje

data Habilidad = UnaHabilidad {
    nombreHabilidad :: String,
    efecto :: (Personaje -> Personaje)
} deriving (Show)

chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso guantelete universo
    | estaCompleto guantelete = take mitadDeUniverso universo
    | otherwise               = universo
    where mitadDeUniverso = div (length universo) 2

estaCompleto :: Guantelete -> Bool
estaCompleto guantelete = (length . gemas) guantelete == 6 && material guantelete == "uru"

---- PUNTO 2 ----

aptoParaPendex :: Universo -> Bool
aptoParaPendex = any ((<45) . edad)

energiaTotal :: Universo -> Float
energiaTotal = sum . map energia . filter ((>1) . length . habilidades)

------ SEGUNDA PARTE ------

---- PUNTO 3 ----

laMente :: Float -> Gema
laMente valor = modificarEnergia valor

elAlma :: Habilidad -> Gema
elAlma habilidad = modificarEnergia (-10) . eliminarHabilidad (nombreHabilidad habilidad)

elEspacio :: String -> Gema
elEspacio planetaNuevo = modificarEnergia (-20) . cambiarPlaneta planetaNuevo

elPoder :: Gema
elPoder = dejarSinEnergia . quitarSiTienePocasHabilidades

elTiempo :: Gema
elTiempo personaje = modificarEnergia (-50) . modificarEdad (negate $ edad personaje `div` 2) $ personaje

laGemaLoca :: Gema -> Gema
laGemaLoca gema = gema . gema

-- aux --

modificarEnergia :: Float -> Personaje -> Personaje
modificarEnergia valor personaje = personaje {energia = energia personaje + valor}

eliminarHabilidad :: String -> Personaje -> Personaje
eliminarHabilidad habilidadAEliminar personaje = personaje {habilidades = filter ((/=habilidadAEliminar) . nombreHabilidad) (habilidades personaje)}

cambiarPlaneta :: String -> Personaje -> Personaje
cambiarPlaneta nuevoPlaneta personaje = personaje {planeta = nuevoPlaneta}

dejarSinEnergia :: Personaje -> Personaje
dejarSinEnergia personaje = modificarEnergia (negate . energia $ personaje) personaje

quitarSiTienePocasHabilidades :: Personaje -> Personaje
quitarSiTienePocasHabilidades personaje
    | length (habilidades personaje) <= 2 = personaje {habilidades = []}
    | otherwise                           = personaje {habilidades = habilidades personaje}

modificarEdad :: Int -> Personaje -> Personaje
modificarEdad valor personaje = personaje {edad = max 18 (valor + edad personaje)}

---- PUNTO 4 -----

usarMjolnir :: Habilidad
usarMjolnir = UnaHabilidad "usarMjolrnir" id

programacionEnHaskell :: Habilidad
programacionEnHaskell = UnaHabilidad "programacionEnHaskell" id

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = UnGuantelete "goma" [elTiempo, elAlma usarMjolnir, laGemaLoca (elAlma programacionEnHaskell)]

---- PUNTO 5 ----

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas personaje = foldl (flip aplicarGema) personaje gemas

aplicarGema :: Gema -> Personaje -> Personaje
aplicarGema gema personaje = gema personaje

---- PUNTO 6 ----

gemaMasPoderosaDeGuantelete :: Guantelete -> Personaje -> Gema
gemaMasPoderosaDeGuantelete guantelete personaje = gemaMasPoderosa (gemas guantelete) personaje

gemaMasPoderosa :: [Gema] -> Personaje -> Gema
gemaMasPoderosa [gema] _ = gema
gemaMasPoderosa (gema1:gema2:gemas) personaje
    | (energia . gema1) personaje < (energia . gema2) personaje = gemaMasPoderosa (gema1:gemas) personaje
    | otherwise                                                 = gemaMasPoderosa (gema2:gemas) personaje