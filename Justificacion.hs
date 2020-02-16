module Justificacion (justificar) where

import System.IO
import Data.List

mostrarHechos :: [[Char]] -> IO ()
mostrarHechos hechos = mapM_ putStrLn hechos

formateaRegla :: Int -> [[[[Char]]]] -> [Char]
formateaRegla idx bc = (show idx) ++ " - " ++ concat (intersperse "&" ((bc !! idx) !! 0)) ++ "->" ++ (((bc !! idx) !! 1) !! 0)

imprimirReglas :: [Int] -> [[[[Char]]]] -> IO ()
imprimirReglas reglas bc = mapM_ (\x -> putStrLn (formateaRegla x bc)) reglas

justificar hechosIniciales hechosFinales reglas bc = do
    putStrLn "Debido a que ingresaste:"
    mostrarHechos hechosIniciales
    putStrLn "Se pudieron aplicar las reglas:"
    imprimirReglas reglas bc
    putStrLn "Y se encontro que tienes:"
    mostrarHechos hechosFinales
