-- LISTAS !!!
{-Existen funciones predefinidas en el Prelude que nos permiten manejar listas por ej: head / tail / !! (infija, devuelve el elemento en la posición i, base 0). Ejemplos 
head [2,4,6,8] = 2 
tail [2,4,6,8] = [4,6,8] 
[2,4,6,8] !! 1 = 4 	-- base 0!! 
null [] = True --Indica si una lista está vacía. 
null [2,4,5] = False 
concat [[1..4],[11..13],[21,34]] = [1,2,3,4,11,12,13,21,34] -- ”aplana” una lista de listas. 

Si se quiere calcular el promedio, dada una lista números y se está usando prelude puro, se puede hacer algo así: 
> sum [3,5,6] / fromInteger(toInteger(length[3,5,6])) 
4.66666666666667 
Tener en cuenta que en la notación [a..b] ni a ni b tienen por qué ser constantes, pueden ser cualquier expresión, p.ej 
[1..head [6,3,8]] 		[min (3+4) (3*4)..max (3+4) (3*4)]
-}

----------------------------

{-Ejercicio 1: Definir una función que sume una lista de números. 
Nota: Investigar sum -}
--suma :: Num a => [a] -> a
--NO entendi
-------------------------------------- VER EL DE ARRIBA

{-Durante un entrenamiento físico de una hora, cada 10 minutos de entrenamiento se tomóo la frecuencia cardíaca de uno de los participantes obteniéndose un total de 7 muestras que son las siguientes:
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125] 
Comienza con un frecuencia de 80 min 0. 
A los 10 min la frecuencia alcanza los 100 
A los 20 min la frecuencia es de 120, 
A los 30 min la frecuencia es de 128
A los 40 min la frecuencia es de 130, …etc.. 
A los 60 min la frecuencia es de 125 
frecuenciaCardiaca es un función constante. 
Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la frecuencia cardíaca. 
Main> promedioFrecuenciaCardiaca 
115.285714285714
Definir la función frecuenciaCardiacaMinuto/1, que recibe m que es el minuto en el cual quiero conocer la frecuencia cardíaca, m puede ser a los 10, 20, 30 ,40,..hasta 60. 
Main> frecuenciaCardiacaMomento 30 
128 
Ayuda: Vale definir una función auxiliar para conocer el número de muestra. 
Definir la función frecuenciasHastaMomento/1, devuelve el total de frecuencias que se obtuvieron hasta el minuto m. 
Main> frecuenciasHastaMomento 30 
[80, 100, 120, 128] 
Ayuda: Utilizar la función take y la función auxiliar definida en el punto anterior. 
-}
{-Esto es parte de la FORMA 2:-} 
import Data.List --Tenia que ponerlo arriba de todo el archivo, por eso
--FORMA 1

frecuenciaCardiaca :: [Integer]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125] 
promedioFrecuenciaCardiaca :: [Integer] -> Float
promedioFrecuenciaCardiaca frecuenciaCardiaca = fromInteger (sum frecuenciaCardiaca) / fromInteger(toInteger(length frecuenciaCardiaca))
--Funciona

--FORMA 2
frecuenciaCardiaca' :: [Integer]
frecuenciaCardiaca' = [80, 100, 120, 128, 130, 123, 125] 
promedioFrecuenciaCardiaca' :: [Integer] -> Float
promedioFrecuenciaCardiaca' frecuenciaCardiaca = fromInteger (sum frecuenciaCardiaca) / (genericLength frecuenciaCardiaca)
--Funciona

{-Ejercicio 3: Definir la función esCapicua, si data una lista de listas, me devuelve si la concatenación de las sublistas es una lista capicua
Porque “neuquen” es capicua.
Ayuda: Utilizar concat, reverse. -}

esCapicua :: (Eq a) => [[a]] -> Bool
esCapicua = reverseList . concat  
    where reverseList x = x == reverse x

{-Ejercicio 4:
Se tiene información detallada de la duración en minutos de las llamadas que se llevaron a cabo en un período determinado, 
discriminadas en horario normal y horario reducido. 
duracionLlamadas = (("horarioReducido",[20,10,25,15]),(“horarioNormal”,[10,5,8,2,9,10])). 
a) Definir la función cuandoHabloMasMinutos, devuelve en que horario se habló más cantidad de minutos, en el de tarifa normal o en el reducido. 
-}
duracionLlamadas :: ((String, [Integer]), (String, [Integer]))
duracionLlamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))

cuandoHabloMasMinutos :: ((String, [Integer]), (String, [Integer])) -> String
cuandoHabloMasMinutos (( _ ,duracionesRed) , ( _ , duracionesNor)) 
    | sum duracionesRed > sum duracionesNor = "horarioReducido"
    | otherwise = "horarioNormal"

-- b) Definir la función cuandoHizoMasLlamadas, devuelve en que franja horaria realizó más cantidad de llamadas, en el de tarifa normal o en el reducido. 

cuandoHizoMasLlamadas :: ((String, [Integer]), (String, [Integer])) -> String
cuandoHizoMasLlamadas  (( _ , duracionesRed) , ( _ , duracionesNor))
    | length duracionesRed > length duracionesNor = "horarioReducido"
    | otherwise = "horarioNormal"
--Funciona

{--ORDEN SUPERIOR
Si una función f recibe en algunos de sus argumentos una función entonces f es una función de orden superior. Veamos un ejemplo. 
Si quiero aplicar a un número n una función determinada podría hacer.. 
aplicar f n = f n 
Por ejemplo, le paso como argumento una función aplicada parcialmente: (+3), (4*). 
Main> aplicar (+ 3) 2 
5 
Main> aplicar (4 *) 3 
12 -} 

{-Ejercicio 1: Definir la función existsAny/2, que dadas una función booleana y una tupla de tres elementos devuelve 
                                                                    ↑
                                                                (a -> Bool)
True si existe algún elemento de la tupla que haga verdadera la función. 
-}
existsAny :: (a -> Bool) -> (a,a,a) -> Bool
existsAny f (x, y, z) = f x || f y || f z 
--Funciona

--Ejercico 2: Definir la función mejor/3, que recibe dos funciones y un número, y devuelve el resultado de la función que dé un valor más alto
mejor:: (Ord(a), Ord (b)) => (a->b) -> (a->b) -> a -> b
mejor f g x = max (f x) (g x)


cuadrado :: Int -> Int
cuadrado x = x * x

triple :: Int -> Int
triple = (3*)
--Funciona

--Ejercicio 3: Definir la función aplicarPar/2, que recibe una función y un par, y devuelve el par que resulta de aplicar la función a los elementos del par
aplicarPar :: (a -> b) -> (a,a) -> (b,b)
aplicarPar f (x,y) = (f x , f y)
--Funciona

--Ejercicio 4: Definir la función parDeFns/3, que recibe dos funciones y un valor, y devuelve un par ordenado que es el resultado de aplicar las dos funciones al valor.
parDeFns :: (a -> b) -> (a -> c) -> a -> (b,c) 
--                ↑           ↑           ↑ ↑  
parDeFns f g x = (f x , g x)
--Funciona

{-ORDEN SUPERIOR + LISTAS
existen funciones de orden superior predefinidas que nos permiten trabajar con listas.
Por ej:  Si quiero filtrar todos los elementos de una lista determinada que cumplen una determinada condición puedo utilizar filter. 
paresEntre n1 n2 = filter even [n1..n2] 
                    ↑
Otro Ejemplo de funciones de orden superior predefinidas que se utiliza mucho es el map Si quiero transformar una lista de elementos, puedo hacer: 
sumarN n lista = map (+n) lista 
Suma n a cada elemento de la lista. 
sumarElDobleDeN n lista = map (+ (doble n)) lista 
Aplica el doble a cada elemento de la lista. 

Otras funciones de orden superior: 
all even [2,48,14] = True -- Indica si todos los elementos de una lista cumplen una condición. 
all even [2,49,14] = False 
any even [2,48,14] = True -- Indica si algunos de los elementos de una lista cumplen una condición
-}

--Ejercicio 1: Definir la función esMultiploDeAlguno/2, que recibe un número y una lista y devuelve True si el número es múltiplo de alguno de los números de la lista
esMultiploDeAlguno :: Integral a => a -> [a] -> Bool
esMultiploDeAlguno x lista = any (multiplo x) lista
    where
        multiplo a b = a `mod` b  == 0
--Funciona

--Ejercicio 2: Armar una función promedios/1, que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento. NOTA: utilizar map
promedios ::  Fractional a => [[a]] -> [a]
promedios listas = map promedioLista listas
    where  
        promedioLista lista = sum lista / genericLength lista
--                                              ↑
--                                  funcion del import Data.list    
--Funciona

{-Ejercicio 3: Armar una función promediosSinAplazos que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento, 
excluyendo los que sean menores a 4 que no se cuentan. NOTA: utilizar map-}
promediosSinAplazos ::  (Fractional a, Ord a) => [[a]] -> [a]
promediosSinAplazos listas = filter (>=4) (map promedioLista listas)
    where  
        promedios listas = map promedioLista listas
        promedioLista lista = sum lista / genericLength lista

-- NO FUNCIONA !!!!!!!!!!!!!!!!!

--Ejercicio 4: Definir la función mejoresNotas, que dada la información de un curso devuelve la lista con la mejor nota de cada alumno
mejoresNotas :: Ord a => [[a]] -> [a]
mejoresNotas notas = map mejorSubLista notas 
    where 
        mejorSubLista lista = maximum lista
--Funciona

{-Ejercicio 5: Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True si el alumno aprobó. 
Se dice que un alumno aprobó si todas sus notas son 6 o más-}
aprobo :: (Ord a, Num a) => [a] -> Bool
aprobo notas = all (>=6) notas  
--Funciona

--Otra manera
aprobo' :: (Ord a, Num a) => [a] -> Bool
aprobo' notas = minimum notas >= 6
--Funciona

--Ejercicio 6: Definir la función aprobaron/1, que dada la información de un curso devuelve la información de los alumnos que aprobaron
aprobaron :: (Ord a, Num a) => [[a]] -> [[a]]
aprobaron lista = filter aprobo lista 
--Funciona

--Ejercicio 7: Definir la función divisores/1, que recibe un número y devuelve la lista de divisores
divisores :: Integral a => a -> [a]
divisores n = [x | x <- [1..n], n `mod` x == 0]
--Funcionar

-- Ejercicio 8: Definir la función exists/2, que dadas una función booleana y una lista
-- devuelve True si la función da True para algún elemento de la lista
exists :: (a -> Bool) -> [a] -> Bool
exists f lista = any f lista  
--Funciona

-- Ejercicio 9: Definir la función hayAlgunNegativo/2, 
-- que dada una lista de números y un (…algo…) devuelve True si hay algún nro. negativo

hayAlgunNegativo :: (Ord a, Num a) => [a] -> a -> Bool
hayAlgunNegativo lista _ = any ( < 0) lista   

--Funciona (no entiendo como definir el "algo" igual xd)

-- Ejercicio 10: Definir la función aplicarFunciones/2, que dadas una lista de funciones y un valor cualquiera,
-- devuelve la lista del resultado de aplicar las funciones al valor.

aplicarFunciones :: [a -> b] -> a -> [b]
aplicarFunciones funciones elemento = map (\f -> f elemento) funciones 
--                                          ↑ 
--                                         funcion lambda 

--Funciona

-- Ejercicio 11: Definir la función sumaF/2, que dadas una lista de funciones y un número,
-- devuelve la suma del resultado de aplicar las funciones al número
sumaF :: Num b => [a -> b] -> a -> b
sumaF funciones numero = sum (map (\f -> (f numero)) funciones)

--Funciona 

{-Ejercicio 12: Un programador Haskell está haciendo las cuentas para un juego de fútbol virtual (como el Hattrick o el ManagerZone).
En un momento le llega la información sobre la habilidad de cada jugador de un equipo, que es un número entre 0 y 12, 
y la orden de subir la forma de todos los jugadores en un número entero; p.ej., subirle 2 la forma a cada jugador. 
Ahora, ningún jugador puede tener más de 12 de habilidad; si un jugador tiene 11 y la orden es subir 2, pasa a 12, no a 13; 
si estaba en 12 se queda en 12. Escribir una función subirHabilidad/2 que reciba un número (que se supone positivo sin validar)
y una lista de números, y le suba la habilidad a cada jugador cuidando que ninguno se pase de 12-}

subirHabilidad :: (Ord a, Num a)=> a -> [a] -> [a]
subirHabilidad aumento habilidad = map (\x -> min (x + aumento) 12 ) habilidad
--                                          |--------------|   
--                                                  ↑
--Esta parte toma la habilidad actual 'x', le suma el 'aumento' y calcula el minimo entre ese resultado y 12, si el resultado < 12, devolvera el resultado, sino devolvera 12

--Funciona

{-Ejercicio 13: Ahora el requerimiento es más genérico: hay que cambiar la habilidad de cada jugador según una función que recibe 
la vieja habilidad y devuelve la nueva. Armar: una función flimitada que recibe una función f y un número n, y devuelve f n garantizando que
quede entre 0 y 12 (si f n < 0 debe devolver 0, si f n > 12 debe devolver 12)
-}
flimitada :: (Ord b, Num b) => (a -> b) -> a -> b 
flimitada funcion numero  
    | (funcion numero) < 0  = 0
    | funcion numero > 12 = 12
    | otherwise = funcion numero 
--Funciona

-- Habia q hacerlo sin guardas xd

--Manera sin guardas. Ayuda: usar min max
flimitada' :: (Ord b, Num b) => (a -> b) -> a -> b  
flimitada' funcion numero = max ( min ( funcion numero ) 12 ) 0  
-- claramente no se me ocurrio a mi xd, un bardo pensarlo asi. Calcula el maximo entre (el minimo del rdo y 12 ) y 0. Por lo que si
-- la funcion es > 12, primero va a devolver 12 por que calculo el minimo entre el rdo y 12 y luego va a calcular el maximo entre 0 y 12 que es 12
-- Si el numero es < 0, en la func de adentro va a devolver ese num < 0 y despues cuando calcula el max entre ese num y 0, devuelve 0.

--Ejercicio 14: Definir una función cambiarHabilidad/2, que reciba una función f y una lista de habilidades,
-- y devuelva el resultado de aplicar f con las garantías de rango que da flimitada.
cambiarHabilidad :: (Ord b , Num b)=> (a -> b) -> [a] -> [b]
cambiarHabilidad funcion habilidades =  map ((\x -> flimitada x) funcion) habilidades
--funciona
--mas facil xd
cambiarHabilidad' :: (Ord b , Num b)=> (a -> b) -> [a] -> [b]
cambiarHabilidad' funcion habilidades =  map (flimitada funcion) habilidades
-- Funciona
-- no entendi mucho este ejercicio , pero weno

