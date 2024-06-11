{-
Las carreras de autos pueden ser muy divertidas, pero tienen consecuencias. En esta edición de parcial vamos a analizar y producir los efectos que sufren los autos al correr una carrera. Los autos se componen de marca, modelo, desgaste (ruedas y chasis, son dos números), velocidad máxima (m/s), y el tiempo de carrera, que lo vamos a considerar inicialmente 0 y tendremos en cuenta luego el uso.

Una pista está compuesta de distintas partes (curvas, rectas, boxes), donde cada tramo termina realizando una transformación sobre el auto que la atraviesa.

Nota: Maximizar el uso de aplicación parcial, composición y orden superior. No usar recursividad a menos que se indique que está permitido.

Modelar el auto, teniendo en cuenta la información necesaria que lo representa. Y luego representar:
Auto Ferrari, modelo F50, sin desgaste en su chasis y ruedas, con una velocidad máxima de 65 m/s.
Auto Lamborghini, modelo Diablo, con desgaste 7 de chasis y 4 de ruedas, con una velocidad máxima de 73 m/s.
Auto Fiat, modelo 600, con desgaste 33 de chasis y 27 de ruedas, con una velocidad máxima de 44 m/s.

-}

data Auto = UnAuto {
    marca  :: String,
    modelo :: String,
    desgaste :: (Float, Float),
    velocidadMaxima :: Float,
    tiempoDeCarrera :: Float
} deriving (Show)

data Pista = UnaPista {
    tramos :: [Tramo]
}

-- Punto 1 --

ferrari :: Auto
ferrari = UnAuto "Ferrari" "F50" (0, 0) 65 0

lamborghini :: Auto
lamborghini = UnAuto "Lamborghini" "Diablo" (7, 4) 73 0

fiat :: Auto
fiat = UnAuto "Fiat" "600" (50, 27) 44 0

-- Punto 2 --

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado auto = desgasteChasis auto < 40 && desgasteRuedas auto < 60

desgasteChasis :: Auto -> Float
desgasteChasis = fst . desgaste

desgasteRuedas :: Auto -> Float
desgasteRuedas = snd . desgaste

noDaMas :: Auto -> Bool
noDaMas auto = not (desgasteChasis auto < 80 && desgasteRuedas auto < 80)

-- Punto 3 --

cambiarDesgasteChasis :: Float -> Auto -> Auto
cambiarDesgasteChasis valor auto = auto {desgaste = (valor, desgasteRuedas auto)}

cambiarDesgasteRuedas :: Float -> Auto -> Auto
cambiarDesgasteRuedas valor auto = auto {desgaste = (desgasteChasis auto, valor)}

repararAuto :: Auto -> Auto
repararAuto auto = cambiarDesgasteChasis bajaDeChasis . cambiarDesgasteRuedas 0 $ auto
    where bajaDeChasis = desgasteChasis auto * 85/100

-- Punto 4 ---

sumarTiempo :: Float -> Auto -> Auto
sumarTiempo valor auto = auto {tiempoDeCarrera = tiempoDeCarrera auto + valor}

type Tramo = Auto -> Auto

-- a --

curva :: Float -> Float -> Tramo
curva angulo longitud auto = sumarTiempo tiempoAVariar . cambiarDesgasteRuedas bajaDeRuedas $ auto
    where 
        bajaDeRuedas = 3*longitud/angulo
        tiempoAVariar = longitud / (velocidadMaxima auto / 2)

curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300

curvaTranca :: Tramo
curvaTranca = curva 110 550

-- b --

tramoRecto :: Float -> Tramo
tramoRecto longitud auto = sumarTiempo tiempoAVariar . cambiarDesgasteChasis bajaDeChasis $ auto
    where 
        tiempoAVariar = longitud / velocidadMaxima auto
        bajaDeChasis = longitud / 100

tramoRectoClassic :: Tramo
tramoRectoClassic = tramoRecto 750 

tramito :: Tramo
tramito = tramoRecto 280

-- c --

tramoBoxes :: Float -> Tramo
tramoBoxes longitud auto
    | estaEnBuenEstado auto = sumarTiempo longitud auto
    | otherwise          = sumarTiempo 10 . repararAuto $ auto

-- d --

estaMojada :: Tramo -> Tramo
estaMojada tramo auto = sumarTiempo mitadTiempo . tramo $ auto
    where mitadTiempo = ((tiempoDeCarrera . tramo) auto - tiempoDeCarrera auto)/ 2

-- e --

tieneRipio :: Tramo -> Tramo
tieneRipio tramo = tramo . tramo

-- f --

tieneObstruccion :: Float -> Tramo -> Tramo
tieneObstruccion metroDePista tramo = tramo . cambiarDesgasteRuedas (2*metroDePista)

-- Punto 5 --

pasaPorTramo :: Tramo -> Auto -> Auto
pasaPorTramo tramo auto
    | not . noDaMas $ auto = tramo auto
    | otherwise            = auto

-- Punto 6 --

superPista :: Pista
superPista = UnaPista [tramoRectoClassic, curvaTranca, estaMojada tramito, tramito, tieneObstruccion 2 $ curva 80 400, curva 115 650, tramoRecto 970, tieneRipio tramito, tramoBoxes 800 . tramoRecto 800 ]

-- b --

pegaLaVuelta :: Pista -> Auto -> Auto
pegaLaVuelta pista auto = foldl (flip pasaPorTramo) auto (tramos pista)

peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta pista autos = map (pegaLaVuelta pista) autos

-- Punto 7 --

-- a --

data Carrera = UnaCarrera {
    pista :: Pista,
    numeroDeVueltas :: Int
}

-- b --

tourBuenosAires :: Carrera
tourBuenosAires = UnaCarrera superPista 20

-- c --

hacerCarrera :: [Auto] -> Carrera -> [[Auto]]
hacerCarrera autos carrera = take (numeroDeVueltas carrera) . iterate (peganLaVuelta (pista carrera)) $ autos