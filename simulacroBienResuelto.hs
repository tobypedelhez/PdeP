import Data.Array.Base (unsafeNewArraySTUArray_)
import GHC.IO.Exception (untangle)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Control.Monad.ST.Unsafe (unsafeDupableInterleaveST)
import System.Win32.DebugApi (PID)
-------------
-- Punto 1 --
-------------

ferrari = Auto "Ferrari" "F50" (0, 0) 65 0
lambo = Auto "Lamborghini" "Diablo" (7, 4) 73 0
fitito = Auto "Fiat" "600" (33, 27) 44 0

data Auto = Auto{
    marca :: String,
    modelo :: String,
    desgaste :: Desgaste,
    velocidadMaxima :: Float,
    tiempo :: Float
} deriving (Show)

type Desgaste = (Float, Float)
chasis :: Desgaste -> Float
chasis = fst

ruedas :: Desgaste -> Float
ruedas = snd

-------------
-- Punto 2 --
-------------

--a--
estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado = estaBien . desgaste

estaBien :: Desgaste -> Bool
estaBien unDesgaste =  chasis unDesgaste <40 && ruedas unDesgaste < 60

--b--
noDaMas :: Auto -> Bool
noDaMas = estaMuyMal . desgaste

estaMuyMal :: Desgaste -> Bool
estaMuyMal unDesgaste = chasis unDesgaste > 80 || ruedas unDesgaste > 80

-------------
-- Punto 3 --
-------------

cambiarDesgasteChasis :: (Float -> Float) -> Auto -> Auto
cambiarDesgasteChasis f unAuto = unAuto{
    desgaste = ((f . chasis . desgaste) unAuto , ruedas (desgaste unAuto))
}

cambiarDesgasteRuedas :: (Float -> Float) -> Auto -> Auto
cambiarDesgasteRuedas f unAuto = unAuto {
    desgaste = ( chasis (desgaste unAuto), (f . ruedas . desgaste) unAuto)
}

reparar :: Auto -> Auto
reparar      = cambiarDesgasteChasis (*0.15) . cambiarDesgasteRuedas (const 0)

-------------
-- Punto 4 --
-------------

--Un tramo es una parte de una pista que transforma un auto (causa efectos en los autos)
type Tramo = Auto -> Auto

--a--
curva :: Float -> Float -> Tramo
curva unAngulo unaLongitud unAuto =
    cambiarDesgasteRuedas (+ desgasteCurva) . aumentarTiempo segundos $ unAuto
    where
        desgasteCurva = 3 * unaLongitud / unAngulo
        segundos = unaLongitud / velocidadMaxima unAuto / 2
aumentarTiempo :: Float -> Auto -> Auto
aumentarTiempo unosSegundos unAuto = unAuto {
     tiempo = tiempo unAuto + unosSegundos
}
--a.1--
curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300

--a.2--
curvaTranca :: Tramo
curvaTranca = curva 110 550

--b--
recta :: Float -> Tramo
recta unaLongitud unAuto =
     cambiarDesgasteChasis (+ (unaLongitud / 100)) . aumentarTiempo segundos $ unAuto
     where 
        segundos = unaLongitud / velocidadMaxima unAuto

--b.1--
rectaClasica :: Tramo
rectaClasica = recta 750

--b.2--
tramito :: Tramo
tramito = recta 200

--c--
boxes :: Tramo -> Tramo
boxes unTramo unAuto 
    | estaEnBuenEstado unAuto = unTramo unAuto --Si esta en buen estado, sigue por el tramo que compone al boxes
    | otherwise               = aumentarTiempo 10 . reparar $ unAuto

--d--
mojar :: Tramo -> Tramo
mojar unTramo unAuto = aumentarTiempo segundosPorMojado . unTramo $ unAuto 
    where
        segundosPorMojado  = (tiempo (unTramo unAuto) - tiempo unAuto) / 2 --hace la diferencia de tiempo en tramo y tiempo original y a eso lo divide entre 2

--d--
ripio :: Tramo -> Tramo
ripio unTramo       = unTramo . unTramo             -- el doble de un tramo
--             unAuto                   $unAUto

--f--
obstruccion :: Tramo -> Float -> Tramo
obstruccion unTramo unaLongitud unAuto = cambiarDesgasteRuedas (+ (unaLongitud *2)). unTramo $ unAuto

-------------
-- Punto 5 --
-------------

pasarPorTramo ::  Auto -> Tramo -> Auto
pasarPorTramo unAuto unTramo  
    | noDaMas unAuto = unAuto -- no pasa nada
    | otherwise     = unTramo unAuto

-------------
-- Punto 6 --
-------------

type Pista = [Tramo]

--a-- 
superPista :: Pista
superPista = [ rectaClasica,
    curvaTranca,
    (tramito. mojar tramito), --2 tramitos consecutivos, pero el primero mojado
    obstruccion (curva 80 400) 2 ,
    curva 115 650 ,
    recta 970 , 
    curvaPeligrosa ,
    ripio tramito , 
    boxes (recta 800) ]
    
peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta unaPista unosAutos = filter (not . noDaMas). map (pegaUnaVuelta unaPista) unosAutos 

pegaUnaVuelta:: Pista -> Auto -> Auto
pegaUnaVuelta unaPista unAuto = foldl pasarPorTramo unAuto unaPista

-------------
-- Punto 7 --
-------------
--a--
data Carrera = Carrera {
    pista :: Pista,
    vueltas :: Int,
}

--b--
tourDeBuenosAires :: Carrera
tourDeBuenosAires = Carrera superPista 20

--c-- 

correrUnaCarrera :: [Autos] -> Carrera -> [[Auto]]
correrUnaCarrera unosAutos unaCarrera = take (vueltas . unaCarrera) . iterate (peganLaVuelta (pista unaCarrera)) $ unosAutos
