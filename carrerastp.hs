{-
Queremos armar un programa que nos permita simular unas fantásticas carreras de autos en las cuales cada vehículo avanza tan rápido como puede para consagrarse campeón, aprovechando del uso de algunos poderes especiales (o power ups) que encuentren a lo largo del trayecto para sacar ventaja por sobre los demás autos.

De cada auto conocemos su color (que nos servirá para identificarlo durante el desarrollo de la carrera), la velocidad a la que está yendo y la distancia que recorrió, ambos valores de tipo entero.

De la carrera sólo nos interesa el estado actual de los autos que están participando, lo cual nos permitirá analizar cómo viene cada uno, y posteriormente procesar aquellos eventos que se den en la carrera para determinar el resultado de la misma.

Teniendo en cuenta lo descrito anteriormente se pide resolver los siguientes puntos explicitando el tipo de cada función desarrollada y utilizando los conceptos aprendidos del Paradigma Funcional, poniendo especial énfasis en el uso de Composición, Aplicación Parcial y Orden Superior.
-}

{-
1. Declarar los tipos Auto y Carrera como consideres convenientes para representar la información indicada y definir funciones para resolver los siguientes problemas:
  a. Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10.
  b. Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va ganando a todos (por haber recorrido más distancia que los otros).
  c. Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que le van ganando.
-}

data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distanciaRecorrida :: Int
} deriving (Show, Eq)

data Carrera = UnaCarrera {
    autos :: [Auto]
} deriving (Show, Eq)

carrera1 :: Carrera
carrera1 = UnaCarrera [car1, car2, car3, car4]

car1 :: Auto
car1 = UnAuto "rojo" 130 120

car2 :: Auto
car2 = UnAuto "verde" 110 250

car3 :: Auto
car3 = UnAuto "azul" 110 12

car4 :: Auto
car4 = UnAuto "violeta" 160 241

distancia :: Auto -> Auto -> Int
distancia auto1 auto2 = distanciaRecorrida auto1 - distanciaRecorrida auto2

-- Punto a

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = (&& distintos) . (<10) . abs $ distancia auto1 auto2
    where 
        distintos = color auto1 /= color auto2

-- Punto b

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto0 carrera0 = (&& primero) . not . all (estaCerca auto0) . autos $ carrera0
    where 
        primero = all (>=0) . map (distancia auto0) . autos $ carrera0

-- Punto c

puestoDeAuto :: Auto -> Carrera -> Int
puestoDeAuto auto0 carrera0 = (+1) . length . filter autoAtras $ autos carrera0
    where autoAtras auto1 = distancia auto0 auto1 < 0

{-
2. Desarrollar las funciones necesarias para manipular el estado de los autos para que sea posible:
  a. Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo indicada, la distancia recorrida por el auto debería ser equivalente a la distancia que llevaba recorrida + ese tiempo * la velocidad a la que estaba yendo.
  b.
    i. A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto de modo que su velocidad final sea la resultante de usar dicho modificador con su velocidad actual.
    ii. Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad indicada de modo que se le reste a la velocidad actual la cantidad indicada, y como mínimo quede en 0, ya que no es válido que un auto quede con velocidad negativa
-}

-- Punto a

correr :: Auto -> Int -> Auto
correr auto0 tiempo = auto0 {distanciaRecorrida = distanciaRecorrida auto0 + tiempo * velocidad auto0}

-- Punto b i (CONCEPTO INTERESANTE; MODIFICADOR)

modificarVelocidad :: (Int->Int) -> Auto -> Auto
modificarVelocidad modificador auto0 = auto0 {velocidad = modificador $ velocidad auto0}

-- ii

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidad auto0 = modificarVelocidad funcionBajar auto0
    where funcionBajar v = max 0 (v-cantidad)

-- (se puede utilizar una funcion lambda en vez de un where)

{-
3. Como se explicó inicialmente sobre las carreras que queremos simular, los autos que participan pueden gatillar poderes especiales a los que denominamos power ups.
Estos poderes son variados y tienen como objetivo impactar al estado general de la carrera, ya sea afectando al auto que lo gatilló y/o a sus contrincantes dependiendo de qué poder se trate.

Nota: disponemos de una función afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a] que puede ser de utilidad para manipular el estado de la carrera. Ver pág. 2 para más detalles.

Inicialmente queremos poder representar los siguientes power ups, pero debería ser fácil incorporar más power ups a futuro para enriquecer nuestro programa:
  a. terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan su velocidad en 50.
  b. miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad los autos que se vean afectados por su uso. Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando.
  c. jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.
  Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el tiempo indicado y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
  Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo.

Como se mencionó anteriormente, disponemos de la siguiente función para usar dentro de la resolución:
-}

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a] --FUNCION IMPORTANTE
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- Punto a

type PowerUp = Carrera -> Carrera

terremoto :: Auto -> PowerUp
terremoto auto0 carrera0 = carrera0 {autos= afectarALosQueCumplen (estaCerca auto0) (bajarVelocidad 50) (autos carrera0)}

-- Punto b

miguelitos :: Int -> Auto -> PowerUp
miguelitos n auto0 carrera0 = carrera0 {autos = afectarALosQueCumplen leVaGanando (bajarVelocidad n) (autos carrera0)}
    where leVaGanando auto = distancia auto0 auto < 0

-- Punto c

jetpack :: Int -> Auto -> PowerUp
jetpack tiempo auto0 carrera0 = carrera0 {autos = afectarALosQueCumplen esElAuto () (autos carrera0)}