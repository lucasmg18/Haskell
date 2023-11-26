import System.Random
import System.IO -- libreria entrada salida
import Data.Char -- librería tipo char
import Data.Tuple -- libreria para las tuplas

----TIPOS NUEVOS

data Wheel = Wheel Int [Int] deriving Show


---------------CONFIGURACION INICIAL ----------------------


--funcion para crear una configuracion inicial de las ruedas de cifrado, de impresion y del tambor
initialConf = do
    putStrLn ("-----Hagelin c52-----")

    --Preguntar posicion inicial de las 6 ruedas elegidas al azar
    newStdGen
    gen <- getStdGen
    let posRuedas = chooseBetween 6 [25, 26, 29, 31, 34, 37, 38, 41, 42, 43, 46, 47] gen
    i0 <- askInitPin (posRuedas !! 0) 
    i1 <- askInitPin (posRuedas !! 1)
    i2 <- askInitPin (posRuedas !! 2)
    i3 <- askInitPin (posRuedas !! 3)
    i4 <- askInitPin (posRuedas !! 4)
    i5 <- askInitPin (posRuedas !! 5)

    --Crear las 6 ruedas
    newStdGen
    gen <- getStdGen
    let ruedas = initialWheels [(posRuedas !! 0, i0),(posRuedas !! 1, i1),(posRuedas !! 2, i2),(posRuedas !! 3, i3),(posRuedas !! 4, i4),(posRuedas !! 5, i5)] gen

    
    let txt0 = "Ruedas elegidas: " ++ (show posRuedas) ++ "\n"
    let txt1 = txt0 ++ "Ruedas: " ++ (show ruedas) ++ "\n"

    --Crear tambor con una configuracion valida de orejetas
    newStdGen
    gen <- getStdGen
    let listaNumDrums = chooseDrum gen
    let tambor = createDrum listaNumDrums


    let txt2 = txt1 ++ "Numeros del algoritmo elegidos para el tambor: " ++ (show listaNumDrums) ++ "\n"
    let txt3 = txt2 ++ "Tambor: " ++ (show tambor) ++ "\n"


    --Crear las ruedas de impresion
    desplAsk <- askInitOffSet
    let despl = desplAsk
    let ruedaImp1 = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']
    let ruedaImpAux = ['A', 'Z', 'Y', 'X', 'W', 'V', 'U', 'T', 'S', 'R', 'Q', 'P', 'O', 'N', 'M', 'L', 'K', 'J', 'I', 'H', 'G', 'F', 'E', 'D', 'C', 'B']
    let ruedaImp2 = rotatePrintWheelRight despl ruedaImpAux

    
    let txt4 = txt3 ++ "Desplazamiento: " ++ (show despl) ++ "\n"
    let txt5 = txt4 ++ "Rueda de impresion 1: " ++ (show ruedaImp1) ++ "\n"
    let txt6 = txt5 ++ "Rueda de impresion 2: " ++ (show ruedaImp2) ++ "\n"


    --Imprimir configuracion en un fichero .txt
    putStrLn "Elige el nombre (con el .txt) del fichero donde guardar la configuracion"
    nameTxt <- getLine
    writeInFile nameTxt txt6

    {-
    --SE PODRIA AÑADIR EL CARACTER ESPACIO ' ' Y CIFRAR CON ESE CARACTER NUEVO
    let ruedaImp1Space = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', ' ']
    let ruedaImp2SpaceAux = ['A', ' ', 'Z', 'Y', 'X', 'W', 'V', 'U', 'T', 'S', 'R', 'Q', 'P', 'O', 'N', 'M', 'L', 'K', 'J', 'I', 'H', 'G', 'F', 'E', 'D', 'C', 'B']
    let ruedaImp2Space = rotatePrintWheelRight despl ruedaImp2SpaceAux
    -}

    putStrLn ("-----Hagelin c52-----")




-------- FUNCIONES CIFRAR/DESCIFRAR ----------



--Funcion que dado un texto y una configuracion de ruedas y tambor deuvuelve el texto cifrado o descifrado por pantalla o por fichero
ciphDeciphText::String -> [Wheel] -> [[Int]] -> [Char] -> [Char] ->IO ()
ciphDeciphText text wheels drum pw1 pw2 =do
    let tUpper = convertToUppercase text
    let tFinal = replaceCharFor ' ' 'X' tUpper
    putStrLn "Elige opcion:"
    putStrLn "1: Imprimir por pantalla texto cifrado modo normal (fijo)"
    putStrLn "2: Imprimir por pantalla y por fichero modo normal (fijo)"
    putStrLn "3: Imprimir por pantalla texto cifrado modo variable"
    putStrLn "4: Imprimir por pantalla y por fichero modo variable"
    putStrLn "0: Salir del programa"
    line1 <- getLine 
    if line1 == "0"
        then return ()
        else
            do
                case line1 of
                    "1" -> putStrLn (ciphDeciph tFinal wheels drum pw1 pw2)
                    "2" -> 
                        do
                            putStrLn "Elige el nombre (con el .txt) del fichero donde guardar el texto"
                            nameTxt <- getLine
                            let tCiph = ciphDeciph tFinal wheels drum pw1 pw2
                            writeInFile nameTxt tCiph
                            putStrLn tCiph
                    "3" -> putStrLn(ciphDeciphVariable tFinal wheels drum pw1 pw2)
                    "4" -> 
                        do
                            putStrLn "Elige el nombre (con el .txt) del fichero donde guardar el texto"
                            nameTxt <- getLine
                            let tCiph = ciphDeciphVariable tFinal wheels drum pw1 pw2
                            writeInFile nameTxt tCiph
                            putStrLn tCiph
                    _ -> putStrLn "La primera opcion elegida no es valida"



 
--Funcion descifrar cifrar en modo fijo (como parametros se le introduce el texto a cifrar, las ruedas de cifrar, el tambor y las dos ruedas de impresion)
ciphDeciph :: String -> [Wheel] -> [[Int]] -> [Char] -> [Char]-> String
ciphDeciph [] _ _ _ _ = []
ciphDeciph text wheels drum pw1 pw2 =
    do
        
        let (auxPW1,auxPW2) = rotatePrintWheelsUntil (head text) pw1 pw2 --Girar ambas ruedas de impresion hasta la letra head text
        let offSetPrint = numOffSet wheels drum  --Desplazamiento de las ruedas de impresion fijas
        let listRot = listUpdateWheels wheels [1,0,0,0,0,0] --Cuanto hay que mover cada rueda, la rueda 1 siempre se mueve
        let nwheels = rotateListedWheels  wheels listRot
        let (npw1,npw2) = rotatePrintWheels offSetPrint auxPW1 auxPW2 --Girar ambas ruedas de impresion el desplazamiento marcado
        (head npw2) : ciphDeciph (tail text) nwheels drum npw1 npw2 --devolver la letra que hay en la pos[0] de la rueda dos(esa sera la letra cifrada))



 
--Funcion descifrar cifrar en modo variable
ciphDeciphVariable :: String -> [Wheel] -> [[Int]] -> [Char] -> [Char]-> String
ciphDeciphVariable [] _ _ _ _ = []
ciphDeciphVariable text wheels drum pw1 pw2 =
    do
        
        let (auxPW1,auxPW2) = rotatePrintWheelsUntil (head text) pw1 pw2  -- Girar ambas ruedas de impresion hasta la letra head text
        let offSetPrint = numOffSet wheels drum  -- Desplazamiento de la segunda rueda de impresion
        let listRot = listUpdateWheels wheels [1,0,0,0,0,0]  -- Cuanto hay que mover cada rueda, la rueda 1 siempre se mueve
        let nwheels = rotateListedWheels  wheels listRot
        let npw2 = rotatePrintWheelRight offSetPrint auxPW2  -- Ahora solo gira la segunda rueda
        (head npw2) : ciphDeciphVariable (tail text) nwheels drum auxPW1 npw2  -- devolver la letra que hay en la pos[0] de la rueda dos(esa sera la letra cifrada))




--Funcion para sustituir un cracter de un texto por otro, por ejemplo los espacios ' ' por 'X'
replaceCharFor :: Char ->Char ->String -> String
replaceCharFor old new s = map (\x -> if x == old then (toUpper new) else x) s


--Funcion para pasar a mayusculas un string
convertToUppercase :: String -> String
convertToUppercase s = map toUpper s






-----------------   FUNCIONES ENTRADA SALIDA FICHERO   -------------------





writeInFile :: String -> String -> IO()
writeInFile nombre texto = writeFile nombre (texto)



--Pide el desplazamiento incial de las dos ruedas de imprimir
askNameTxt::IO String
askNameTxt =
    do
        putStrLn ("Elige el nombre (con el .txt) del fichero donde guardar la configuracion")
        pos <- getLine 
        return pos 







-----------------     FUNCIONES TAMBOR         -----------------






----Funciones de calcular para el cifrado




--Funcion que calcula el numero de barras que se han desplazado a la izquierda en todo el tambor que sera lo que se muevan las ruedas de impresion
numOffSet :: [Wheel] -> [[Int]] -> Int
numOffSet [] [] = 0
numOffSet (w:ws) (d:ds) = s + numOffSet ws ds
    where 
        s = if(isActivePin w)
                then sum (take 27 d) --Solo las 27 barras primeras, las otras 5 son de desplazamiento
                else 0





----Funciones de crear




-- A partir de la lista de numeros del algoritmo crea el tambor que es una lista de listas con x 1s y 27-x 0s y al final de cada columna las barras de desplazamiento siguiendo una configuracion adecuada de orejetas proporcionado en la pagina web de la explicación de la maquina
createDrum :: [Int] -> [[Int]]
createDrum [] = []
createDrum (x:xs) = l : createDrum xs
    where 
        l = replicate x 1 ++ replicate (27 - x) 0 ++ replicate (5 - length xs - 1) 0 ++ replicate (length xs ) 1



-- funcion que verifica que estan todos los numeros del 0 al 25 estan en la lista
--verifyDrum :: [Int]
verifyDrum :: [Int] -> Bool
verifyDrum l = all (`elem` l) [0..25]



--A partir de un generador elige una lista de numeros entre 1 y 14 que cumplen las condiciones del algoritmo para colocarl las orejetas
chooseDrum :: StdGen -> [Int]
chooseDrum gen =
    do
        let l = chooseBetweenUntil 6 27 [1..14] gen in
            let s = sumAllBinary 63 l in
                if(verifyDrum s)
                    then l
                    else let (_, ngen) = random gen :: (Int, StdGen) in
                        chooseDrum ngen



-- Elige n numeros al azar de la lista hasta que sume x
-- numero de elecciones -> numero x que tienen que sumar -> lista
chooseBetweenUntil :: Int -> Int -> [Int] -> StdGen -> [Int]
chooseBetweenUntil n x l gen = 
    do 
        let nl = chooseBetween n l gen in
            if((sum nl) == x)
                then nl
                else let (_, ngen) = random gen :: (Int, StdGen) in
                   chooseBetweenUntil n x l ngen





----- Funciones calculo auxiliares




--devuelve un array de int con la representacion binaria del numero y el array del tamaño dado
binarioArray :: Int -> Int -> [Int]
binarioArray 0 _ = []
binarioArray cont 0 = binarioArray (cont - 1) 0 ++ [0]
binarioArray cont n = binarioArray (cont - 1)(n `div` 2) ++ [n `mod` 2]

--crea un array con las sumas de los numeros de la lista multiplicados por la representacion binaria del numero actual
-- Si la suma es mayor que 25 le resta 26 como indica el algoritmo
sumAllBinary :: Int -> [Int] -> [Int]
sumAllBinary 0 _ = [0]
sumAllBinary n l  = sumAllBinary (n-1) l ++ [s]
    where 
        aux = sum (zipWith (*) l (binarioArray (length l) n))
        s = if(aux > 25)
                then aux - 26
                else aux








------- FUNCIONES RUEDAS -----------








---- Funciones para crear ruedas cifrado




--Elige n entre los elementos de la lista y devuelve los elegidos en una lista
chooseBetween :: Int -> [Int] -> StdGen -> [Int]
chooseBetween 0 _ _ = []
chooseBetween n l g = (l !! (pos - 1)) : chooseBetween (n-1) nl ng
    where
        (pos, ng) = randomR (1, (length l) ) g
        nl = take(pos - 1) l ++ drop pos l


-- Numero de pines anteriores activos seguidos 
pinsWheel :: Int -> StdGen -> [Int]
pinsWheel num genR = p : pinsWheel newNum newGenR
    where
        (randomN,newGenR) = randomR (0, 1) genR
        p = if (num > 2) then 0 else randomN
        newNum = if (p == 0) then 0 else num + 1


--Crea una lista de ruedas a partir de una lista de (numero de pins y el inicial) y de generadores de numeros aleatorios
initialWheels :: [(Int,Int)] -> StdGen -> [Wheel]
initialWheels [] _ = []
initialWheels (x:xs) g = w : initialWheels xs ng
    where 
        w = Wheel (snd x) (take (fst x) (pinsWheel 0 g))
        (_, ng) = random g :: (Int, StdGen)






----Funciones modificar ruedas de cifrado





--Devuelve una lista con los pasos que hay que mover cada rueda las ruedas lo que le corresponde a cada una con la configuracion de orejetas proporcionada
--Si esta activa la rueda n, todas las ruedas siguientes n + 1, n + 2, n + 3... se mueven un paso
--Se debe llamar inicialmente a esta funcion con una lista de 0s de igual tamaño que la lista de Wheel pero con el primer valor 1 ya que la rueda 1 siempre se mueve
listUpdateWheels :: [Wheel] -> [Int] -> [Int]
listUpdateWheels []  []= []
listUpdateWheels (w:ws) (n:ns)= n : listUpdateWheels ws ms 
    where 
        ms = if(isActivePin w)
                then map (+ 1) ns
                else ns


--funcion que rota las ruedas segun la lista de rotaciones que se pase con un valor para cada rueda
rotateListedWheels ::[Wheel] -> [Int] -> [Wheel]
rotateListedWheels []  []= []
rotateListedWheels (w:ws) (n:ns) = nw :rotateListedWheels ws ns
    where 
        nw = rotateWheel n w


--Rota n veces todas las ruedas del array de ruedas
rotateAllWheels :: Int -> [Wheel] -> [Wheel]
rotateAllWheels _ [] = []
rotateAllWheels n (w:ws) = nw : rotateAllWheels n ws
    where 
        nw = rotateWheel n w


--funcion que avanza la posicion de la rueda
rotateWheel :: Int -> Wheel -> Wheel
rotateWheel n (Wheel p list) = Wheel ((p+n) `mod` (length list)) list 





---- Funciones modificar ruedas de impresion





--funcion que rota las dos ruedas de impresion hasta llegar a la letra elegida
rotatePrintWheelsUntil :: Char -> [Char] ->[Char] -> ([Char],[Char])
rotatePrintWheelsUntil c p1 p2 =
    do
        if(head p1 == c)
            then (p1, p2)
            else 
                let np1 = rotatePrintWheelRight 1 p1 in
                let np2 = rotatePrintWheelRight 1 p2 in
                    rotatePrintWheelsUntil c np1 np2


--funcion que rota las dos ruedas de impresion n veces
rotatePrintWheels :: Int -> [Char] ->[Char] -> ([Char],[Char])
rotatePrintWheels n p1 p2 =(rotatePrintWheelRight n p1, rotatePrintWheelRight n p2)


--funcion que rota el numero de pasos dado una lista que represente una rueda de impresion (hacia la derecha)
rotatePrintWheelRight :: Int -> [Char] -> [Char]
rotatePrintWheelRight n l = drop (length l - n) l ++ take (length l - n) l


--funcion que rota el numero de pasos dado una lista que represente una rueda de impresion (hacia la derecha)
rotatePrintWheelLeft :: Int -> [Char] -> [Char]
rotatePrintWheelLeft n l = drop n l ++ take n l





---- Funciones consultar ruedas de cifrado





--funcion que comprueba si el pin actual de la rueda esta activo en una rueda
isActivePin :: Wheel -> Bool
isActivePin (Wheel v l) = (( l !! v) == 1)





---- Funciones preguntar al usuario valores para la configuracion inicial




--Pide un pin inicial para una rueda en especifico
askInitPin::Int -> IO Int
askInitPin n =
    do
        putStrLn ("Elige posicion inicial de la rueda " ++ (show n) ++ " (Introduce numero entre 1 y " ++ (show n)++")")
        pos <- getInt 
        let p = pos - 1
        if(pos <= n && pos > 0) 
            then return p  --En fisico empiezan las ruedas en 1 y en el codigo en 0
            else 
                do
                    putStrLn ("Numero introducido fuera del rango")
                    askInitPin n


--Pide el desplazamiento incial de las dos ruedas de imprimir
askInitOffSet::IO Int
askInitOffSet =
    do
        putStrLn ("Elige el desplazamiento inicial de las ruedas de impresión (Introduce numero mayor o igual a 0)")
        pos <- getInt 
        if(pos >= 0) 
            then return pos
            else 
                do
                    putStrLn ("Numero introducido fuera del rango")
                    askInitOffSet 







-----------------  Funciones Auxiliares -------------------------------





getInt :: IO Int --Pedir el numero 
getInt = do
    n <- getLine
    return (read n::Int)




