import System.IO

main = do
    hSetBuffering stdin LineBuffering
    let regla = []
    doLoop

doLoop = do
    
    putStrLn "Escribe h -> crear BH \nEscribe c -> crear BC \nEscribe i -> hacer inferencia\nq -> salir "
    opcion <- getLine
    case opcion of
        'q':_->return(){--}
        'h':_ -> do putStrLn ("Editando BH ")
                    putStrLn ("Ingresa tus hechos a la BH ")
                    item <- getLine 
                    a <- hGetChar handle
                    appendFile "bh.txt" (item ++ ",") {-Ingresa hechos-}
                    contents <- readFile "bh.txt"
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
