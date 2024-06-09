import Control.Concurrent.STM (lengthTBQueue)
import Data.ByteString.Unsafe (unsafePackMallocCString)
-- LINK ENUNCIAD : https://docs.google.com/document/d/e/2PACX-1vQX84Z8tKK_1tZtS27zFcqovm8zwTUSPDmPqJvyC5IoODbk9YQtLxxbfAftwLBwFH7a3J3WDz0BRg9k/pub

------------------------
-- DEFINICIONES DADAS --
------------------------

data Persona = Persona {
  nombrePersona :: String,
  suerte :: Int,
  inteligencia :: Int,
  fuerza :: Int
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)

-------------
-- Punto 1 --
-------------

niveles :: Persona -> [Int]
niveles unaPersona = [fuerza unaPersona, suerte  unaPersona,  inteligencia unaPersona]
--a--
sumaDeNiveles :: Persona -> Int
sumaDeNiveles = sum . niveles

--b--
diferenciaDeNiveles :: Persona -> Int
diferenciaDeNiveles unaPersona = maximoNivel unaPersona - minimoNivel unaPersona

maximoNivel = maximum . niveles
minimoNivel = minimum . niveles

--c-- nivelesMayoresA n, que indica la cantidad de niveles de la persona que están por encima del valor dado.
nivelesMayoresA :: Int -> (Persona -> Int)
nivelesMayoresA n =  length . filter (>n). niveles 

-------------
-- Punto 2 --
-------------

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion     = concat . map efectos. ingredientes 

-------------
-- Punto 3 --
-------------

--a--
pocionesHardcore :: [Pocion] -> [String]
pocionesHardcore    = map nombrePocion . filter condicionHardcore 

condicionHardcore :: Pocion -> Bool
condicionHardcore = (>=4) . length . efectosDePocion 

--b--
cantidadDePocionesProhibidas :: [Pocion] -> Int
cantidadDePocionesProhibidas    = length . filter esProhibida

esProhibida :: Pocion -> Bool
esProhibida     = any ( flip elem nombresDeIngredientesProhibidos . nombreIngrediente) . ingredientes

--c--

todasDulces :: [Pocion] -> Bool
todasDulces = all ( any ((== "azucar") . nombreIngrediente). ingredientes)

-------------
-- Punto 4 --
-------------

tomarPocion :: Pocion -> Persona -> Persona
-- type Efecto = Persona -> Persona
tomarPocion unaPocion personaInicial = foldl (\persona efectos -> efectos persona) personaInicial . efectosDePocion $ unaPocion
--                                     foldl (\persona efectos -> efectos persona) personaInicial (efectosDePocion unaPocion) 

-------------
-- Punto 5 --
-------------

esAntidotoDe :: Pocion -> Pocion -> Persona -> Bool
esAntidotoDe unaPocion unAntidoto unaPersona = ( (==unaPersona). tomarPocion unAntidoto . tomarPocion unaPocion)  unaPersona

-------------
-- Punto 6 --
-------------

personaAfectada :: Pocion -> (Persona -> Int) -> ([Persona] -> Persona)
personaAfectada unaPocion unCriterio = maximoSegun (unCriterio . tomarPocion unaPocion)

--Ejemplo de uso: 
personaAfectada (Pocion "Placebo" []) sumaDeNiveles  [...]