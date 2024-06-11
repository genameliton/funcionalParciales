data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)
type Situacion = [Aspecto]

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> [Aspecto] -> Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo :: String -> [Aspecto] -> Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto :: Aspecto -> [Aspecto] -> [Aspecto]
reemplazarAspecto aspectoBuscado situacion =
    aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)


situacion1 :: Situacion
situacion1 = [UnAspecto "incertidumbre" 30, UnAspecto "peligro" 2, UnAspecto "tension" 100]

situacion2 :: Situacion
situacion2 = [UnAspecto "tension" 101, UnAspecto "incertidumbre" 31, UnAspecto "peligro" 3]

-- 1a --

cambiarGrado :: Float -> Aspecto -> Aspecto
cambiarGrado valor aspecto = aspecto {grado = valor}

modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto func aspecto = flip cambiarGrado aspecto . func . grado $ aspecto

-- b --

situacionMejorQueOtra :: Situacion -> Situacion -> Bool
situacionMejorQueOtra situac1 situac2 = all (aspectoMejorQueSituacion situac1) situac2

aspectoMejorQueSituacion :: Situacion -> Aspecto -> Bool
aspectoMejorQueSituacion situacion aspecto = mejorAspecto aspecto $ buscarAspecto aspecto situacion

-- c --

modificarAspectoEnSituacion :: String -> Situacion -> (Float -> Float) -> Aspecto
modificarAspectoEnSituacion tipo situacion funcion = cambiarGrado nuevoGrado . buscarAspectoDeTipo tipo $ situacion
    where nuevoGrado = funcion . grado . buscarAspectoDeTipo tipo $ situacion

modificarSituacion :: String -> (Float -> Float) -> Situacion -> Situacion
modificarSituacion tipo funcion situacion  = reemplazarAspecto (modificarAspectoEnSituacion tipo situacion funcion) situacion

{-
Modelar a las Gemas de modo que estén compuestas por su nombre, la fuerza que tienen y la personalidad. La personalidad de una Gema debe representar cómo reacciona ante una situación, derivando de ese modo a una situación diferente.
Definir las siguientes personalidades:
vidente: ante una situación disminuye a la mitad la incertidumbre y baja en 10 la tensión.
relajada: disminuye en 30 la tensión de la situación y, dependiendo de qué tan relajada sea la Gema, aumenta el peligro en tantas unidades como nivel de relajamiento tenga.
Mostrar ejemplos de cómo se crean una Gema vidente y una Gema descuidada.
-}

-- 2b --

data Gema = UnaGema {
    nombre :: String,
    fuerza :: Float,
    personalidad :: Personalidad
}

cambiarNombre :: String -> Gema -> Gema
cambiarNombre nuevoNombre gem1 = gem1 {nombre = nuevoNombre}

cambiarFuerza :: Float -> Gema -> Gema
cambiarFuerza nuevoValor gem1 = gem1 {fuerza = nuevoValor}

cambiarPersonalidad :: Personalidad -> Gema -> Gema
cambiarPersonalidad nuevaPersonalidad gem1 = gem1 {personalidad = nuevaPersonalidad}

type Personalidad = Situacion -> Situacion

-- b --

vidente :: Personalidad
vidente = modificarSituacion "incertidumbre" (/2) . modificarSituacion "tension" (subtract 10)

relajada :: Float -> Personalidad
relajada relajacion = modificarSituacion "tension" (subtract 30) . modificarSituacion "peligro" (+ relajacion)

-- c --

gema1 :: Gema
gema1 = UnaGema "Garnet" 50 vidente

gema2 :: Gema
gema2 = UnaGema "Perla" 30 (relajada 100)

-- 3 --

leGanaAOtra :: Situacion -> Gema -> Gema -> Bool
leGanaAOtra situacion gem1 gem2 = leGanaEnFuerza gem1 gem2 && situacionMejorQueOtra (personalidad gem1 situacion) (personalidad gem2 situacion)

leGanaEnFuerza :: Gema -> Gema -> Bool
leGanaEnFuerza gem1 gem2 = fuerza gem1 >= fuerza gem2

-- 4 --

nombreFusion :: Gema -> Gema -> String
nombreFusion gem1 gem2
    | nombre gem1 == nombre gem2 = nombre gem1
    | otherwise                  = nombre gem1 ++ nombre gem2

personalidadFusion :: Gema -> Gema -> Personalidad
personalidadFusion gem1 gem2 = personalidad gem2 . personalidad gem1 . modificarSituacion "incertidumbre" (subtract 10) . modificarSituacion "tension" (subtract 10) . modificarSituacion "peligro" (subtract 10)

fusionCompatible :: Situacion -> Gema -> Gema -> Bool
fusionCompatible situacion gem1 gem2 = situacionMejorQueOtra situacionFusionada situacionGema1 && situacionMejorQueOtra situacionFusionada situacionGema2
    where 
        situacionFusionada = personalidadFusion gem1 gem2 situacion
        situacionGema1 = personalidad gem1 situacion
        situacionGema2 = personalidad gem2 situacion

fuerzaFusion :: Situacion -> Gema -> Gema -> Float
fuerzaFusion situacion gem1 gem2
    | fusionCompatible situacion gem1 gem2 = (fuerza gem1 + fuerza gem2) * 10
    | otherwise                  = fuerza (gemaDominante situacion gem1 gem2) * 7

gemaDominante :: Situacion -> Gema -> Gema -> Gema
gemaDominante situacion gem1 gem2
    | leGanaAOtra situacion gem1 gem2 = gem1
    | otherwise                       = gem2

fusion :: Situacion -> Gema -> Gema -> Gema
fusion situacion gem1 gem2 = UnaGema (nombreFusion gem1 gem2) (fuerzaFusion situacion gem1 gem2) (personalidadFusion gem1 gem2) 

-- 5 --

fusionGrupal :: Situacion -> [Gema] -> Gema
fusionGrupal _ [x] = x
fusionGrupal situacion gemas = foldl1 (fusion situacion) gemas  

-- 6 --