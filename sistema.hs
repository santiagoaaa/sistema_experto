import System.IO
import Data.List.Split

--hace split  de antecedente y consecuente
splitClausula delimAntecedentes delimConsecuente str = [splitOn delimAntecedentes (head (splitOn delimConsecuente str)), tail (splitOn delimConsecuente str)]

--hace split de las reglas con #
--splitArchivo::[Char] -> [Char] -> [Char] -> [Char] -> [[[[Char]]]]
splitArchivo delim delimAntecedentes delimConsecuente str = map (splitClausula delimAntecedentes delimConsecuente) (splitOn delim str)

{-
    let file = "abc.txt" 
    writeFile file "I am just experimenting here." 
    readFile file
-}
main = do
    hSetBuffering stdin LineBuffering
    doLoop


doLoop = do
    
    putStrLn "Escribe h -> crear BH \nEscribe c -> crear BC \nEscribe i -> hacer inferencia\nq -> salir "
    opcion <- getLine
    case opcion of
        'q':_->return(){--}
        'h':_ -> do putStrLn ("Editando BH ")
                    putStrLn ("Ingresa tus hechos a la BH ")
                    item <- getLine
                    appendFile "bh.txt" (item ++ ",") {-Ingresa hechos-}
                    --contents <- readFile "bh.txt"
                    putStrLn ("*Hechos registrados*")
                    doLoop

        'c':_ -> do putStrLn ("Editando BC")
                    putStrLn ("Ingresa conocimiento a la BC")
                    item <- getLine 
                    appendFile "bc.txt" (item ++ "#") {-Ingresa conocimiento-}
                    putStrLn ("*Conocimiento registrado*")
                    doLoop

        'i':_ -> do putStrLn ("Motor de inferencia")
                    putStrLn ("Comienza...")
                    contents <- readFile "bc.txt"
                    print $ splitArchivo "#" "," "-" contents --Esto regresa la lista de listas de la BC
                    doLoop
                    
