---- PUNTO 1 ----

-- a --

data Pais = UnPais{
    ingresoPerCapita :: Float,
    poblacionActivaPublico :: Float,
    poblacionActivaPrivado :: Float,
    recursosNaturales :: [RecursoNatural],
    deudaConFMI :: Float
} deriving (Show)

type RecursoNatural = String
type Receta = Pais -> Pais

-- b --

namibia :: Pais
namibia = UnPais 4140 400000 650000 ["mineria", "ecoturismo"] 50000000

---- PUNTO 2 ----

prestarMillones :: Float -> Receta
prestarMillones n = aumentarDeuda (n*1.5)

reducirPuestosDeTrabajoPublicos :: Float -> Receta
reducirPuestosDeTrabajoPublicos cantidad = reducirActivosPublico cantidad . modificarIngresoPerCapita (*0.8)

explotar :: RecursoNatural -> Receta
explotar recurso = disminuirDeuda 2000000 . quitarRecurso recurso

establecerBlindaje :: Receta
establecerBlindaje pais = prestarMillones (pbi pais) . reducirPuestosDeTrabajoPublicos 500 $ pais

pbi :: Pais -> Float
pbi pais = ingresoPerCapita pais * (poblacionActivaPrivado pais+ poblacionActivaPublico pais)

-- Aux --

aumentarDeuda :: Float -> Pais -> Pais
aumentarDeuda cantidad pais = pais {deudaConFMI = deudaConFMI pais + cantidad}

disminuirDeuda :: Float -> Pais -> Pais
disminuirDeuda cantidad pais = pais {deudaConFMI = deudaConFMI pais - cantidad}

reducirActivosPublico :: Float -> Pais -> Pais
reducirActivosPublico n pais = pais {poblacionActivaPublico = poblacionActivaPublico pais - n}

modificarIngresoPerCapita :: (Float -> Float) -> Pais -> Pais
modificarIngresoPerCapita func pais = pais {ingresoPerCapita = func (ingresoPerCapita pais)}

quitarRecurso :: RecursoNatural -> Pais -> Pais
quitarRecurso recurso pais = pais {recursosNaturales = filter (/= recurso) (recursosNaturales pais)}

---- PUNTO 3 ----

-- a --

recetaDoble :: Receta
recetaDoble = explotar "mineria" . prestarMillones 200

-- b --

namibiaModificado :: Pais
namibiaModificado = recetaDoble namibia

---- PUNTO 4 ---- con orden superior, composicion y aplicacion parcial

-- a --

puedeZafar :: [Pais] -> [Pais]
puedeZafar  = filter $ elem "petroleo" . recursosNaturales

-- b --

fmiAFavor :: [Pais] -> Float
fmiAFavor = foldr ((+) . deudaConFMI) 0

---- PUNTO 5 ----

dePeorAMejor :: Pais -> [Receta] -> Bool
dePeorAMejor _ [receta] = True
dePeorAMejor pais (receta1:receta2:recetas) = pbi (receta1 pais) < pbi (receta2 pais) && dePeorAMejor pais recetas 

-- PUNTO 6 -- 

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

