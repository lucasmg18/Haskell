module Main where

    import System.IO -- libreria entrada salida
    import Data.Char -- librería tipo char
    import Data.Tuple -- libreria para las tuplas
    import Data.Bits -- librería para realizar operaciones binarias

        

    --Funciones para obtener elementos de tuplas:

    --tuplas de 3 elementos
    fst3:: (a,b,c) -> a --obtener primer elemento de una tupla
    fst3 (a,_,_) = a
    snd3:: (a,b,c) -> b --obtener segundo elemento de una tupla
    snd3 (_,b,_) = b
    thrd3:: (a,b,c) -> c --obtener tercer elemento de una tupla
    thrd3 (_,_,c) = c


    --Funcion obtener numero por entrada

    getInt :: IO Int --Pedir el numero 
    getInt = do
        n <- getLine
        return (read n::Int)



    main = do
        putStrLn ("-----Algoritmo extendido de euclides-----")
        progOpciones
    

    -- Algoritmo de Stein Binario


    -- mcd = m*u + n*v
    -- Parametros m - n - um - vm - un - vn - a - b (al inicializar con (m n 1 0 0 1) cumplen que: m = nMayor*um + nMenor*vm y n = nMayor*un + nMenor*vn )
    bxeucAux :: Int -> Int-> Int-> Int-> Int-> Int -> Int -> Int-> (Int, Int, Int)
    bxeucAux m 0 um vm un vn a b = ( m, um, vm)
    bxeucAux 0 n um vm un vn a b = ( n, un, vn) 
    bxeucAux m n um vm un vn a b = 
        do
            if(even m && even n)
                then 
                    let sol = bxeucAux (shiftR m 1) (shiftR n 1) um vm un vn a b in
                    (shiftL (fst3 sol) 1, snd3 sol, thrd3 sol) -- devuelve 2*mcd, u y v tal que mcd(aAnt, bAnt) = 2*mcd(aSig=aAnt/2, bSig = bAnt/2) y mcd = 2*mcd = 2*(aSig=aAnt/2)*u + 2*(bSig = bAnt/2)*v
                else 
                    if (even m && odd n)
                        then 
                            if(even um && even vm)
                                then bxeucAux (shiftR m 1) n (shiftR um 1) (shiftR vm 1) un vn a b -- um y vm pares por tanto se puede divivir entre dos
                                else bxeucAux (shiftR m 1) n (shiftR (um + b) 1) (shiftR (vm - a) 1) un vn a b -- um y vm al menos uno impar y por las precondiciones sabemos que al menos a o b impar, como m es par se cumple que (um + b) y (vm -a) son pares y se puede dividir por 2
                        else 
                            if (odd m && even n)
                                then 
                                    if(even un && even vn)
                                        then bxeucAux m (shiftR n 1) um vm (shiftR un 1) (shiftR vn 1) a b -- un y vn pares por tanto se puede divivir entre dos
                                        else bxeucAux m (shiftR n 1) um vm (shiftR (un + b) 1) (shiftR (vn - a) 1) a b -- un y vn al menos uno impar y por las precondiciones sabemos que al menos a o b impar, como n es par se cumple que (un + b) y (vn -a) son pares y se puede dividir por 2
                                else 
                                    if(m > n)
                                        then bxeucAux (m - n) n (um - un) (vm - vn) un vn a b
                                        else bxeucAux m (n - m) um vm (un - um) (vn - vm) a b 

    --Divide a y b por 2 hasta que uno sea impar
    elimPotDos :: Int -> Int -> (Int, Int, Int)
    elimPotDos a b = 
        do
            if(even a && even b)
                then let sol = elimPotDos (shiftR a 1) (shiftR b 1) in
                      (fst3 sol, snd3 sol,(thrd3 sol) + 1)
                else   (a, b, 0)
    --Multiplica x por 2 "c" veces
    multPotDos :: Int -> Int -> Int
    multPotDos a  0 = a
    multPotDos a  c = multPotDos (shiftL a 1) (c -1)

    bxeuc :: Int -> Int -> (Int, Int, Int)
    bxeuc a b = (multPotDos (fst3 sol) (thrd3 simp), snd3 sol, thrd3 sol)
        where 
            simp = elimPotDos a b
            sol = bxeucAux (fst3 simp) (snd3 simp) 1 0 0 1 (fst3 simp) (snd3 simp) -- Se inicializa a 1 0 para m y 0 1 para n porque: m = 1*m + 0*n y n = 0*m + 1*n



    -- Funciones manejo de opciones
    

    mostrarResultado :: Int -> Int -> (Int, Int, Int) -> IO()
    mostrarResultado m n res = 
        do 
            putStrLn ("El maximo comun divisor es " ++ (show (fst3 res)) ++ ", u es: " ++ (show (snd3 res) )++ ", v es:" ++ (show (thrd3 res)))
            putStrLn ("Cumpliendo: " ++ (show m) ++ " * " ++ (show (snd3 res)) ++ " + "  ++ (show n) ++ " * " ++ (show (thrd3 res)) ++ " = " ++ (show (fst3 res)) )
            if((fst3 res) == m*(snd3 res) +n* (thrd3 res))
                then putStrLn ("(La igualdad es correcta)" )
                else putStrLn ("(Error: La igualdad no es correcta)" )

            



    progOpciones::IO ()
    progOpciones =
        do
            putStrLn "Elige opcion:"
            putStrLn "b:  Ejecutar funcion binaria (Stein)"
            putStrLn "e:  Salir del programa"
            line <- getLine 
            if line == "e"
                then return ()
                else
                    do
                        putStrLn "Introduce el primer numero:"
                        num1 <- getInt
                        putStrLn "Introduce el segundo numero:"
                        num2 <- getInt
                        let nMayor = max num1 num2
                        let nMenor = min num1 num2
                        case line of
                            "b" -> mostrarResultado nMayor nMenor (bxeuc nMayor nMenor)
                            _ -> putStrLn "La opcion elegida no es valida"
                        progOpciones

