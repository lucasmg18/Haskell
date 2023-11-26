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
    --Tuplas de 6 elementos
    sel1of6:: (a,b,c,d,e,f) -> a --obtener primer elemento de una tupla
    sel1of6 (a,_,_,_,_,_) = a
    sel2of6:: (a,b,c,d,e,f) -> b --obtener segundo elemento de una tupla
    sel2of6 (_,b,_,_,_,_) = b
    sel3of6:: (a,b,c,d,e,f) -> c --obtener tercer elemento de una tupla
    sel3of6 (_,_,c,_,_,_) = c
    sel4of6:: (a,b,c,d,e,f) -> d --obtener cuarto elemento de una tupla
    sel4of6 (_,_,_,d,_,_) = d
    sel5of6:: (a,b,c,d,e,f) -> e --obtener quinto elemento de una tupla
    sel5of6 (_,_,_,_,e,_) = e
    sel6of6:: (a,b,c,d,e,f) -> f --obtener sexto elemento de una tupla
    sel6of6 (_,_,_,_,_,f) = f


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
        





    -- ESTILO RECURSIVO



    -- mcd = m*u + n*v
    -- Parametros m - n - um - vm - un - vn (al inicializar con (m n 1 0 0 1) cumplen que: m = nMayor*um + nMenor*vm y n = nMayor*un + nMenor*vn )
    rxeucAux :: Int -> Int-> Int-> Int-> Int-> Int-> (Int, Int, Int)
    rxeucAux m 0 um vm un vn = ( m, um, vm) 
    rxeucAux m n um vm un vn = rxeucAux n (mod m n) un vn (um - coc*un) (vm - coc*vn)
        where
            coc = div m n

    rxeuc :: Int -> Int -> (Int, Int, Int)
    rxeuc a b = rxeucAux a b 1 0 0 1 -- Se inicializa a 1 0 para m y 0 1 para n porque: m = 1*m + 0*n y n = 0*m + 1*n

    -- ESTILO RECURSIVO mas simple, versión 2

    rxeucSimp :: Int-> Int-> (Int, Int, Int)
    rxeucSimp a 0 = ( a, 1, 0) --Devuelve: a, u=0 y v=1   tal que n * u(=0) + a * v(=1) = a  (en la anterior llamada como a(n+1) = b(n): mcd = a(n+1) = b(n) = a(n)*0 + b(n)*1 )
    rxeucSimp a b = (fst3 sol, thrd3 sol, (snd3 sol) - (div a b) * (thrd3 sol))
        where
            sol = rxeucSimp b (mod a b)

    {-
     EXPLICACION ALGORITMO: 
     
     sol = rxeucSimp b (mod a b) 
     la funcion devuelve el mcd = sol[1], u = sol[2] y v = sol[3] tal que:
     mcd = sol[1] = a(n)*sol[2] + b(n)*sol[3]   

     entonces como las formulas del siguiente u y del siguiente v son:
     u(k) = u(k-2) - coc*u(k-1),    v(k) = v(k-2) - coc*v(k-1) =>  v(k - 1) = v(k + 1) - coc*v(k)

     aplicando v(k)= u(k-1)  queda: (estableciendo que u(k) = sol[2] y v(k) = sol[3])
     (recordar, la anterior llamada necesita u(k-1) y v(k-1))
     u(k-1) = v(k)  y  v(k - 1) = u(k) - coc*v(k) 
     entonces se devuelve:  ( mcd, v(k) ,   u(k) - coc*v(k) )
     -}



    -- ESTILO ITERATIVO



    {-
        IMPLEMENTACION DE ITERATE (como referencia)
        -- iterate f x = [x,f x, f (f x),...]
        iterate:: (a -> a) -> a -> [a]
        iterate f x = x : iterate f (f x)    
    -}



    -- Estilo Iterativo con una función que crea la tabla del algoritmo extendido de euclides con el siguiente resto, con u y con v (itera sobre la nueva tala que crea como la funcion iterate)
    

    -- Parametros m - n - um - vm - un - vn. La tabla es una lista de tuplas de tres elementos que guarda el m (siguiente resto), u y v tal que m = nMayor*u + nMenor*v
    ixeucAux :: Int -> Int-> Int-> Int-> Int-> Int -> [(Int, Int, Int)]
    ixeucAux m 0 um vm un vn = [(m, um, vm)]
    ixeucAux m n um vm un vn = (resto, umSig, vmSig) : ixeucAux n resto un vn umSig vmSig
        where
            resto = mod m n
            coc = div m n
            umSig = um - coc*un
            vmSig = vm - coc*vn
    
    ixeuc :: Int -> Int -> (Int, Int, Int)
    ixeuc a b = ((fst3 sol), (snd3 sol), (thrd3 sol))
        where
            tabla = ixeucAux a b 1 0 0 1 -- Se inicializa a 1 0 para a y 0 1 para b porque: a = 1*m + 0*n y b = 0*m + 1*n
            sol = last tabla --la solucion esta en la ultima iteracion




    -- Estilo iterativo utilizando la función iterate y una función nueva
    

     -- Funcion para usarla con iterate
    funcIxeuc :: (Int, Int, Int, Int, Int, Int)-> (Int, Int, Int, Int, Int, Int)
    funcIxeuc x = (n, (mod m n), un, vn, (um - coc*un), (vm - coc*vn))
        where
            m = sel1of6 x
            n = sel2of6 x 
            um = sel3of6 x 
            vm = sel4of6 x 
            un = sel5of6 x
            vn = sel6of6 x 
            coc = div m n
    
    -- Devuelve si la iteración que se ha introducido es la final o no
    compFinal :: (Int, Int, Int, Int, Int, Int) -> Bool
    compFinal x = (sel2of6 x) > 0 -- mientras sea mayor que cero todavia no ha acabado el algoritmo

    -- Misma función que takehWhile pero incluye también en la lista que devuelve el último elemento que ha comprobado que no cumple la condicion
    takeWhilePlusOne :: (a -> Bool) -> [a] -> [a]
    takeWhilePlusOne c [] = []
    takeWhilePlusOne c (x:xs) = 
        if c x then x : takeWhilePlusOne c xs
        else [x]
    
    ixeucIterate :: Int -> Int -> (Int, Int, Int)
    ixeucIterate a b = ((sel1of6 sol), (sel3of6 sol), (sel4of6 sol))
        where
            tabla = takeWhilePlusOne compFinal (iterate funcIxeuc (a, b, 1, 0, 0, 1)) 
            sol = last tabla --la solucion esta en la ultima iteracion



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
            putStrLn "r:  Utilizar funcion recursiva"
            putStrLn "i:  Utilizar funcion iterativa"
            putStrLn "t:  Utilizar funcion iterativa que usa la función iterate"
            putStrLn "o:  Utilizar funcion recursiva más simple (versión 2)"
            putStrLn "b:  Ejecutar funcion binaria (Stein)"
            putStrLn "c:  Ejecutar todas las versiones seguidas"
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
                            "r" -> mostrarResultado nMayor nMenor (rxeuc nMayor nMenor) 
                            "i" -> mostrarResultado nMayor nMenor (ixeuc nMayor nMenor)
                            "t" -> mostrarResultado nMayor nMenor (ixeucIterate nMayor nMenor)
                            "o" -> mostrarResultado nMayor nMenor (rxeucSimp nMayor nMenor)
                            "b" -> mostrarResultado nMayor nMenor (bxeuc nMayor nMenor)
                            "c" -> 
                                do
                                    putStrLn "Recursiva (r):"
                                    mostrarResultado nMayor nMenor (rxeuc nMayor nMenor) 
                                    putStrLn "Iterativa (i):"
                                    mostrarResultado nMayor nMenor (ixeuc nMayor nMenor)
                                    putStrLn "Iterativa con iterate (t):"
                                    mostrarResultado nMayor nMenor (ixeucIterate nMayor nMenor)
                                    putStrLn "Recursiva más simple versión 2 (o):"
                                    mostrarResultado nMayor nMenor (rxeucSimp nMayor nMenor)
                                    putStrLn "Binaria (b):"
                                    mostrarResultado nMayor nMenor (bxeuc nMayor nMenor)
                            _ -> putStrLn "La opcion elegida no es valida"
                        progOpciones

