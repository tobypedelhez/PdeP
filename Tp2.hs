--APLICACION PARCIAL !!!!!

--Ejercicio 1: Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese número el 1. 
siguiente :: Int -> Int 
siguiente numero = numero + 1
--Funciona
--Otra manera (aplicacion parcial)
siguienteAp :: Int -> Int
siguienteAp = (+1)

--Ejercicio 2: Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número
mitad :: Fractional a => a -> a
mitad = (/2)
--funciona

--Ejercicio 3: Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa. 
inversa :: Fractional a => a -> a
inversa = (1/)
--Funciona

--Ejercicio 4: Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo.
triple :: Num a => a -> a
triple = (*3) --tambien funciona (3*) xq el producto es conmutativo
--Funciona

--Ejercicio 5: Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true 
--si el número es positivo y false en caso contrario. 
esNumeroPositivo :: (Num a, Ord a) => a -> Bool
esNumeroPositivo = (> 0) 
--Funciona
-- COMPOSICION !!!

--Ejercicio 6: Resolver la función del ejercicio 2 de la guía anterior esMultiploDe/2, utilizando aplicación parcial y composición.
esMultiploDe :: Int -> Int -> Bool
esMultiploDe numerador = (==0) . ( flip mod numerador)
--Funciona

--Ejercicio 7: Resolver la función del ejercicio 5 de la guía anterior esBisiesto/1, utilizando aplicación parcial y composición.
esBisiesto :: Int -> Bool
esBisiesto year = ((esMultiploDe 400 year ) || (esMultiploDe 4 year && (not . esMultiploDe 100)year))
--Funciona

--Ejercicio 8: Resolver la función inversaRaizCuadrada/1, que da un número n devolver la inversa su raíz cuadrada. 
--Nota: Resolverlo utilizando la función inversa Ej. 2.3, sqrt y composición.
inversaRaizCuadrada :: (Fractional a, Floating a) => a -> a
inversaRaizCuadrada numero = ((inversa . sqrt)numero)
--Funciona

--Ejercicio 9: Definir una función incrementMCuadradoN, que invocándola con 2 números m y n, incrementa un valor m al cuadrado de n 
-- Nota: Resolverlo utilizando aplicación parcial y composición. 
incrementMCuadradoN :: Num a => a -> a -> a
incrementMCuadradoN m  = (+ m) . (^2)
--Funciona

--Ejercicio 10: Definir una función esResultadoPar/2, que invocándola con número n y otro m, 
--devuelve true si el resultado de elevar n a m es par. 
--Nota Obvia: Resolverlo utilizando aplicación parcial y composición.
esResultadoPar :: Integral a => a -> a -> Bool
esResultadoPar n = even . ( ^ n)
-- NO FUNCIONA

