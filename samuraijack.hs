data Elemento = UnElemento {
    tipo :: String,
    ataque :: (Personaje -> Personaje),
    defensa :: (Personaje -> Personaje)
}

data Personaje = UnPersonaje {
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
}

type Enemigo = Personaje

---- PUNTO 1 ----

-- a --

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio valor personaje = personaje {anioPresente = valor}

-- b --

meditar :: Personaje -> Personaje
meditar personaje = modificarSalud (salud personaje/2) personaje

modificarSalud :: Float -> Personaje -> Personaje
modificarSalud valor personaje = personaje {salud = max 0 (salud personaje + valor)}

-- c --

causarDanio :: Float -> Personaje -> Personaje
causarDanio valor = modificarSalud (-valor)

---- PUNTO 2 ----

-- a --

esMalvado :: Personaje -> Bool
esMalvado  = any ((== "Maldad") . tipo) . elementos

-- b --

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje) 

-- c --

enemigosMortales :: Personaje -> [Enemigo] -> [Enemigo]
enemigosMortales personaje = filter (loMata personaje)

loMata :: Personaje -> Enemigo -> Bool
loMata personaje enemigo = any (tieneAtaqueMortal personaje) (elementos enemigo)

tieneAtaqueMortal :: Personaje -> Elemento -> Bool
tieneAtaqueMortal personaje elemento = salud (ataque elemento personaje) == 0

---- PUNTO 3 ----

-- a --

concentracion :: Int -> Elemento
concentracion nivelDeConcentracion = UnElemento {
    tipo = "Magia",
    ataque = id,
    defensa = foldr1 (.) (replicate nivelDeConcentracion meditar)
}

-- b --

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad esbirroMalvado

esbirroMalvado :: Elemento
esbirroMalvado = UnElemento "Maldad" (causarDanio 1) id

-- c --

jack :: Personaje
jack = UnPersonaje "Jack" 300 [concentracion 3, katanaMagica] 200

katanaMagica :: Elemento
katanaMagica = UnElemento "Magia" (causarDanio 1000) id

-- d --

aku :: Int -> Float -> Personaje
aku anio cantidadDeSalud = UnPersonaje {
    nombre = "Aku",
    salud = cantidadDeSalud,
    elementos = [concentracion 4, portalAlFuturo anio] ++  esbirrosMalvados (100*anio),
    anioPresente = anio
}

enviarAlFuturo :: Int -> Personaje -> Personaje
enviarAlFuturo anio personaje = mandarAlAnio (anioPresente personaje + anio) personaje

portalAlFuturo :: Int  -> Elemento
portalAlFuturo anio = UnElemento {
    tipo = "Magia",
    ataque = enviarAlFuturo 2800,
    defensa = aku (anio + 2800) . salud
}

---- PUNTO 4 ----

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
    | salud atacante == 0 = (atacante, defensor)
    | otherwise             = luchar atacanteAfectado defensorAfectado 
    where atacanteAfectado = foldl (flip ataque) defensor (elementos atacante)
          defensorAfectado = foldl (flip defensa) atacante (elementos defensor)

---- PUNTO 5 ----
{-
Inferir el tipo de la siguiente función:
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))

En la primer guarda podemos saber que y 0 es del mismo tipo que z, ya que son comparables
:t y = a -> b, :t z = b. La función devuelve otra función que va de lista a lista, por la estructura que tiene map de (a1 -> b1) -> [a1] -> [b1]. También sabemos que aplicarle a x un valor de tipo b devuelve una tupla. Nos va quedando así:
(b -> e -> (d,d)) -> (a -> b) -> b -> [e] -> [d], donde e es el valor que debe tomar x para terminar de transformarse en tupla.
    Como b es comparable, y a es un elemento numérico porque la función y recibe un 0, quedaría:

(Eq b, Num a) => (b -> e -> (d,d)) -> (a -> b) -> b -> [e] -> [d]
-}

f :: (Eq t1, Num t2) => (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))

