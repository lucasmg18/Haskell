module Main where

    import System.IO -- libreria entrada salida
    import Data.Char -- librería tipo char

    getPisos :: IO Int --Pedir el numero de pisos de la torre
    getPisos = do
        putStrLn "Introduce el numero de pisos de la torre de Hanoi"
        n <- getLine
        return (read n)

    main = do
        n_pisos <- getPisos
        putStrLn ("Los movimientos par resolver el puzzle son:")
        pasosTorresHanoi n_pisos

    pasosTorresHanoiAux :: Int -> Char -> Char -> [Char] -> String
    pasosTorresHanoiAux a s dest disc = "mueve el disco " ++ [(disc !! (a-1))] ++ " de la torre " ++ [s] ++ " a la " ++ [dest] -- disc !! n devuelve el elemento n-esimo de disc
    
    -- Parametros de entrada: Numero de pisos - Torre origen - Torre auxiliar - Torre destino - array con los discos en la torre origen
    pasosTorresHanoi' :: Int -> Char -> Char -> Char -> [Char] -> [String]
    pasosTorresHanoi' 0 _ _ _ _ = [] --ningun movimiento
    pasosTorresHanoi' a ori aux dest disc = pasosTorresHanoi' (a-1) ori dest aux disc ++ [pasosTorresHanoiAux a ori dest disc] ++ pasosTorresHanoi' (a-1) aux ori dest disc
    -- Mover n-1 pisos de la torre origen al auxiliar ++ mover la base (el piso mas bajo) del origen al destino ++ Mover n-1 pisos de la torre auziliar al destino  
    pasosTorresHanoi :: Int -> IO()
    pasosTorresHanoi a = putStrLn (unlines (pasosTorresHanoi' a 'A' 'B' 'C' discos)) -- unlines junta strings con saltos de linea en medio
        where
            discos = revertir (map (intToDigit) [1..a]) --map aplicas la funcion a esos parametros, intodigit convertir char a int

    {-revertirAux :: [a] -> [a] -> [a]
    revertirAux lst [] = lst
    revertirAux lst (x:xs) = revertirAux  (x:lst) xs

    revertir :: [a] -> [a]--invertir el orden de una lista de numero
    revertir xs = revertirAux xs []-}

    revertir :: [a] -> [a]
    revertir [] = []
    revertir (x:xs) = reverse xs ++ [x]

