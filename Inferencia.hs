module Inferencia (encadenamiento) where

import Data.List
import System.IO

--bc = [[["a","b"],["p"]],[["b","c"],["m"]],[["d","e","f"],["n"]],[["n","g"],["m"]],[["h","m"],["q"]]]

--bh = [["a","b"]]

-- Regresa los antecedentes que se encuentran en la base de hechos
indices :: [[Char]] -> [[Char]] -> [[Int]]
indices bh clausula = map (\x -> elemIndices x bh) clausula

-- Regresa los índices de los antecedentes que NO se encuentran en la base de hechos
auxFaltantes :: [[Char]] -> [[Char]] -> [Int]
auxFaltantes bh clausula = elemIndices [] (indices bh clausula)

-- Aplica la función 'auxFaltantes' a toda la base de conocimiento
completas :: [[[[Char]]]] -> [[Char]] -> [[Int]]
completas bc bh = map (\c -> auxFaltantes bh (head c)) bc

elementosPorTamano :: Int -> [[x]] -> [x]
elementosPorTamano _ [] = []
elementosPorTamano num (x:xs) =
            if num == length x
                then x
                else elementosPorTamano num xs

-- Obtiene la lista de menor longitud (menos antecedentes le faltan para poder aplicarse)
masCorto :: [[a]] -> [a]
masCorto x = let minlist = minimum (map (length) x)
                in elementosPorTamano minlist x

-- Obtiene los indices de las reglas que tienen la menor cantidad de antecedentes faltantes
siguienteRegla :: [[[[Char]]]] -> [[Char]] -> [Int]
siguienteRegla bc bh = elemIndices (masCorto (completas bc bh)) (completas bc bh)

-- Obtiene una lista de Strings con los antecedentes que le faltan a la regla 'idx' para 
-- poder aplicarse
antecedentesFaltantes :: [[[[Char]]]] -> Int -> [Int] -> [[Char]]
antecedentesFaltantes bc idx faltas = map (\x -> (head (bc !! idx)) !! x) faltas

-- Obtiene el consecuente de la regla 'idx'
aplicar :: [[[[Char]]]] -> Int -> [Char]
aplicar bc idx = ((tail (bc !! idx)) !! 0) !! 0

-- Agrega un nuevo hecho a la base de hechos
actualizar :: [[Char]] -> [Char] -> [[Char]]
actualizar bh hecho = bh ++ [hecho]

-- Si la las reglas que se se pueden aplicar no les falta ningun antecedente, obtiene las reglas a aplicar
-- Si no, DEBERIA de preguntar
equiparacion :: [[[[Char]]]] -> [[Char]] -> [Int]
equiparacion bc bh
    | length (masCorto (completas bc bh)) == 0 = siguienteRegla bc bh
    | otherwise = []
--    | otherwise = pregunta bc bh

-- Realiza el encadenamiento hacia adelante de forma recursiva
encadenamiento bc bh i = do
    let cc = equiparacion bc bh
    if i >= (length cc)
        then bh
        else do
            let idx = cc !! i
            let nuevoHecho = aplicar bc idx
            encadenamiento bc (actualizar bh nuevoHecho) (i + 1)

-- DEBERIA de tomar una clausula a la que le faltan antecedentes en la BH y preguntar por los faltates
--pregunta :: [[[[Char]]]] -> [[Char]] -> [Int]
--pregunta bc bh = do
--    let posibles = siguienteRegla bc bh
--    let faltantes = antecedentesFaltantes bc (posibles !! 0) ((completas bc bh) !! (posibles !! 0))
--    mapM (\x -> preguntaSintoma x bh) faltantes

-- DEBERIA de preguntar si el usuario tiene un sintoma e ingresarlo a la BH si lo tiene
--preguntaSintoma sintoma bh = do
--    print "¿Tienes '" ++ sintoma ++ "'? [s/n]"
--    respuesta <- getLine
--    if respuesta == "s"
--        then actualizar bh sintoma
--        else bh
