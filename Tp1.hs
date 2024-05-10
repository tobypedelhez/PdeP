import Distribution.Utils.NubList (NubList)
import GHC.IO.Handle.FD (fdToHandle)
--Ejercio 1 Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3
esMultiploDeTres :: Int -> Bool
esMultiploDeTres x = x `mod` 3 == 0
--Funciona

--Ejercicio 2 : Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero
esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = b `mod` a  == 0
--Funciona

--Ejercicio 3 : Definir la función cubo/1, devuelve el cubo de un número.
cubo :: Num a => a -> a
cubo x = x*x*x 
--Funciona

-- Ejercicio 4: Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.
area :: Num a => a -> a -> a
area b a = b*a 
-- Funciona 

--Ejercicio 5: Definir la función esBisiesto/1, indica si un año es bisiesto. 
--(Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) 
--Nota: Resolverlo reutilizando la función esMultiploDe/2 
esBisiesto :: Int -> Bool
esBisiesto year = (esMultiploDe 400 year ) || (esMultiploDe 4 year  && not (esMultiploDe 100 year))
--Funciona 

--Ejercicio 6: Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.
celsiusToFahr :: Fractional a => a -> a --Puse fractional xq la temp puede tener ,
celsiusToFahr t = ((t * 9) / 5) + 32
--Funciona 

--Ejercicio 7: Definir la función fahrToCelsius/1, la inversa de la anterior.
fahrToCelsius :: Fractional a => a -> a
fahrToCelsius t = (t - 32) * 5/9

--Ejercicio 8 Definir la función haceFrioF/1,
--indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius.
haceFrioF :: (Ord a, Fractional a) => a -> Bool
haceFrioF t = fahrToCelsius t < 8 
--Funciona 

--Ejercicio 9: Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula. 
--m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)} 
--Nota: Se puede utilizar gcd.
mcm :: Int -> Int -> Int
mcm a b = a * b `div` gcd a b

-- Faltan hacer ejs 10 y 11

-- Ejercicio 12: Este ejercicio alguna vez se planteó como un Desafío Café con Leche: Implementar la función esCuadradoPerfecto/1
--sin hacer operaciones con punto flotante. Ayuda: les va a venir bien una función auxiliar, tal vez de dos parámetros. 
--Pensar que el primer cuadrado perfecto es 0, para llegar al 2do (1) sumo 1, para llegar al 3ro (4) sumo 3, para llegar al siguiente (9) 
--sumo 5, después sumo 7, 9, 11 etc.. También algo de recursividad van a tener que usar. 

chequearNumero:: Integral a => a -> a -> Bool
chequearNumero numero iteracion = (iteracion ^ 2 == numero) || (iteracion ^ 2 < numero && chequearNumero numero (iteracion + 1))


esCuadradoPerfecto:: Integral a => a -> Bool
esCuadradoPerfecto numero = chequearNumero numero 0