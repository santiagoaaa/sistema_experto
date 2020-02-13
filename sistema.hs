import qualified Inferencia as I
import System.IO
import Data.List.Split
import Data.List


{-
    Prelude Data.List.Split> head (drop 1 (splitArchivo "#" "," "-" "a,b,n-m#c,u,p-m#a,m,n-x" ))
    [["c","u","p"],["m"]]
    Prelude Data.List.Split> head ( (splitArchivo "#" "," "-" "a,b,n-m#c,u,p-m#a,m,n-x" ))
-}

--hace split  de antecedente y consecuente
splitClausula delimAntecedentes delimConsecuente str = [splitOn delimAntecedentes (head (splitOn delimConsecuente str)), tail (splitOn delimConsecuente str)]

--hace split de las reglas con #
--splitArchivo::[Char] -> [Char] -> [Char] -> [Char] -> [[[[Char]]]]
splitArchivo delim delimAntecedentes delimConsecuente str = map (splitClausula delimAntecedentes delimConsecuente) (splitOn delim str)

--Funcion para jalar la B
splitArchivobh delim str = splitOn delim str

{-
    let file = "abc.txt" 
    writeFile file "I am just experimenting here." 
    readFile file
-}
main = do
    hSetBuffering stdin LineBuffering
    doLoop


doLoop = do
    putStrLn "Escribe 1 -> crear nueva bh \nEscribe 2 -> ingresar nuevos hechos \nEscribe 3 -> crear nueva bc \nEscribe 4 -> ingresar conocimiento \nEscribe 5 -> ver bc y bh \nEscribe i -> hacer inferencia\nq -> salir "
    opcion <- getLine
    case opcion of
        'q':_->return(){--}
        '1':_ -> do putStrLn ("Creando nueva BH")
                    putStrLn("Ingresa tus hechos a la BH")
                    item <- getLine
                    writeFile "bh.txt" (item)
                    putStrLn("Listo")
                    doLoop
        '2':_ -> do putStrLn ("Editando BH ")
                    putStrLn ("Ingresa tus hechos a la BH ")
                    item <- getLine
                    appendFile "bh.txt" (","++item) {-Ingresa hechos-}
                    --contents <- readFile "bh.txt"
                    putStrLn ("*Hechos registrados*")
                    doLoop

        '3':_ -> do putStrLn ("Creando nueva BC")
                    putStrLn ("Ingresa conocimiento a la BC (a,b,x-x#)")
                    item <- getLine 
                    writeFile "bc.txt" (item) {-Ingresa conocimiento-}
                    putStrLn ("*Conocimiento registrado*")
                    doLoop
        
        '4':_ -> do putStrLn ("Editando BC")
                    putStrLn ("Ingresa conocimiento a la BC (a,b,x-x)")
                    item <- getLine 
                    appendFile "bc.txt" (item ++ "#") {-Ingresa conocimiento-}
                    putStrLn ("*Conocimiento registrado*")
                    doLoop
        '5':_ -> do putStrLn ("Contenido BC")
                    content <- readFile "bc.txt"
                    putStrLn content
                    putStrLn ("Contenido bh")
                    content <- readFile "bh.txt"
                    putStrLn content
                    doLoop


        'i':_ -> do putStrLn ("Motor de inferencia")
                    putStrLn ("Comienza...")
                    contents <- readFile "bc.txt"
                    contentsbh <- readFile "bh.txt"
                    --print $ head ( drop 2 (splitArchivo "#" "," "-" contents))
                    let bc =   splitArchivo "#" "," "-" contents
                    let bh =   splitArchivobh "," contentsbh
                    --putStrLn "Base de conocimiento"
                    --print bc
                    --putStrLn "Base de hechos"
                    --print bh
                    putStrLn "***Resultado de la inferencia***"
                    putStrLn "********************************"
                    print $ I.encadenamiento bc bh 0
                    putStrLn "********************************"
                    doLoop