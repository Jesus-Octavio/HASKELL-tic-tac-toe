import Data.List
import System.IO

data Tablero =  Tab (Bool, [Celda])
    deriving(Show, Eq, Read)

data Celda = P Posicion | F Ficha
    deriving(Show, Eq, Read)

type Posicion = Int

data Ficha = Circulo | Cruz
    deriving(Show, Eq, Read)

type Profundidad = Int

type Valor = Int


tableroInicial :: Bool -> Tablero
tableroInicial b
    | b = Tab (True, [P 0,P 1,P 2,P 3,P 4,P 5,P 6,P 7,P 8]) --empieza el humano
    | otherwise = Tab (False, [P 0,P 1,P 2,P 3,P 4,P 5,P 6,P 7,P 8]) -- empieza el ordenador

tableroCompleto :: Tablero -> Bool
tableroCompleto (Tab (b, xs)) = and (map celdaOcupada xs)

esTablero :: Tablero -> Bool    
esTablero (Tab (b, xs)) = length xs == 9

mostrarCelda :: Celda -> [Char]
mostrarCelda (P n) = show n
mostrarCelda (F Circulo) = "O"
mostrarCelda (F Cruz) = "X"

mostrarTableroLista :: Tablero -> [String]
mostrarTableroLista (Tab (b, xs))
    | esTablero (Tab (b, xs)) = map mostrarCelda xs
    | otherwise = error "No es tablero"

mostrarTableroCadena :: Tablero -> String
mostrarTableroCadena (Tab (b, xs)) = filter (/=' ') (unwords (mostrarTableroLista (Tab (b, xs))))

mostrarLinea :: Int-> Tablero -> String
mostrarLinea n (Tab (b, xs))
    | n == 1 = intersperse '|' (take 3 (mostrarTableroCadena (Tab (b, xs))))
    | n == 2 = intersperse '|' (take 3 (drop 3 (mostrarTableroCadena (Tab (b, xs)))))
    | n == 3 = intersperse '|' (drop 6 (mostrarTableroCadena (Tab (b, xs))))
    | otherwise = error "Solo hay tres lineas"
    
mostrarTablero :: Tablero -> String
mostrarTablero (Tab (b, xs))
    | b =  resto ++ "\n" ++ "Es el turno de HUMANO (CRUZ)"
    | otherwise = resto ++ "\n" ++ "Es el turno de MAQUINA (CIRCULO)"
    where resto= (mostrarLinea 1 (Tab (b, xs))) ++ "\n-----\n" ++ (mostrarLinea 2 (Tab (b, xs))) ++ "\n-----\n" ++ (mostrarLinea 3 (Tab (b, xs)))

mostrarTablero2 :: Tablero -> String --hace lo mismo que la anterior pero sin devolver de quién es el turno
                                     -- la he usado para lo que me has dicho de que devuelva el tablero al final. BORRA ESTO
mostrarTablero2 (Tab (b, xs)) = (mostrarLinea 1 (Tab (b, xs))) ++ "\n-----\n" ++ (mostrarLinea 2 (Tab (b, xs))) ++ "\n-----\n" ++ (mostrarLinea 3 (Tab (b, xs)))


cambiar :: [Celda] -> (Posicion, Celda) -> [Celda]
cambiar xs (i, e) = antes ++ [e] ++ despues
    where (antes, _:despues) = splitAt i xs

ponerCruz :: Tablero -> Posicion -> Tablero
ponerCruz (Tab (b,xs))  p
    | not (esTablero (Tab (b,xs))) = error "No es tablero"
    | celdaOcupada(devuelveCelda p (Tab (b, xs)))= error "Movimiento invalido"
    | otherwise = Tab (False, cambiar xs (p, (F Cruz)))

ponerCirculo :: Tablero -> Posicion -> Tablero
ponerCirculo (Tab (b, xs))  p
    | not (esTablero (Tab (b,xs))) = error "No es tablero"
    | celdaOcupada(devuelveCelda p (Tab (b, xs)))= error "Movimiento invalido"
    | otherwise = Tab (True, cambiar xs (p, (F Circulo)))


ponerFicha :: Tablero -> Posicion -> Tablero
ponerFicha (Tab (b, xs)) p
    | b == True = ponerCruz (Tab (b, xs)) p
    | otherwise = ponerCirculo (Tab (b,xs)) p

devuelveCelda :: Int -> Tablero -> Celda
devuelveCelda n (Tab (b, xs))
    | not (esTablero (Tab (b, xs))) = error "No es tablero"
    | (n < 0) || (n > 8) = error "Celda inexistente"
    | otherwise = xs !! n

celdaOcupada :: Celda -> Bool
celdaOcupada (P _) = False
celdaOcupada (F _) = True

tablerosSiguientes :: Tablero -> [Tablero]
tablerosSiguientes (Tab (b, xs))
    | tableroCompleto (Tab (b, xs)) && hayEmpate (Tab (b, xs)) = []
    | ganaCruz (Tab (b, xs)) =  []
    | ganaCirculo (Tab (b, xs)) =  []
    | b = map (ponerCruz (Tab (b, xs))) [x | x <- map posicionesCeldasLibres xs, x >= 0]
    | otherwise = map (ponerCirculo (Tab (b, xs))) [x | x <- map posicionesCeldasLibres xs, x >= 0]
      
posicionesCeldasLibres :: Celda -> Posicion
posicionesCeldasLibres (P x) = x
posicionesCeldasLibres (F x) = -1

posicionesConCruz :: Tablero -> [Posicion]
posicionesConCruz (Tab (b,xs)) = indice (F Cruz) (zip xs [0..])
    where indice _ [] = []
          indice a ((x,i):xs)
            | x == a = i : indice a xs
            | otherwise = indice a xs

posicionesConCirculo :: Tablero -> [Posicion]
posicionesConCirculo (Tab (b,xs)) = indice (F Circulo) (zip xs [0..])
    where indice _ [] = []
          indice a ((x,i):xs)
            | x == a = i : indice a xs
            | otherwise = indice a xs

subconjunto :: [Posicion] -> [Posicion] -> Bool
subconjunto ys xs = all (`elem` ys) xs

indicesLineas3EnRaya :: [Celda] -> [[Int]]
indicesLineas3EnRaya xs = filas ++ columnas ++ diagonales
        where n = round (sqrt (fromIntegral (length xs)))
              filas =  [[fila * n + i | i <- [0..n - 1]] | fila <- [0..n - 1]]
              columnas =  [[col + i * n | i <- [0..n - 1] ] | col <- [0..n - 1]]
              diagonales = [[(i `mod` n) + i * n | i <- [0..n - 1]], [i+n-1 | i <- [0,n-1..(n-1)*(n-1)]]]

ganaCruz :: Tablero -> Bool
ganaCruz (Tab (b, xs)) = or (map (subconjunto cruces) [x | x <- indicesLineas3EnRaya xs])
                where cruces = posicionesConCruz (Tab (b,xs))

ganaCirculo :: Tablero -> Bool
ganaCirculo (Tab (b, xs)) = or (map (subconjunto circulos) [x | x <- indicesLineas3EnRaya xs])
                where circulos = posicionesConCirculo (Tab (b,xs))

hayGanador:: Tablero -> Bool
hayGanador (Tab (b, xs)) = ganaCirculo (Tab (b, xs)) || ganaCruz (Tab (b, xs))

hayEmpate :: Tablero -> Bool
hayEmpate (Tab (b, xs)) = not (hayGanador (Tab (b, xs)))

mejor :: [Valor] -> Valor
mejor xs = maximum xs

peor :: [Valor] -> Valor
peor xs = minimum xs

hayCruz :: Celda -> Bool
hayCruz (F Cruz) = True
hayCruz _ = False

hayCirculo :: Celda -> Bool
hayCirculo (F Circulo) = True
hayCirculo _ = False

evaluaLineas2x :: [Celda] -> [[Int]] -> Valor
evaluaLineas2x xs [] = 0
evaluaLineas2x xs ([x,y,z]:xss)
    | hayCruz (xs !! x) && hayCruz (xs !! y) && not (celdaOcupada (xs !! z)) || hayCruz (xs !! x) && hayCruz (xs !! z) && not (celdaOcupada (xs !! y)) || hayCruz (xs !! z) && hayCruz (xs !! y) && not (celdaOcupada (xs !! x)) = 100 + evaluaLineas2x xs xss
    | otherwise = evaluaLineas2x xs xss

evaluaLineas1x :: [Celda] -> [[Int]] -> Valor
evaluaLineas1x xs [] = 0
evaluaLineas1x xs ([x,y,z]:xss)
    | hayCruz (xs !! x) && not (celdaOcupada (xs !! y)) && not (celdaOcupada (xs !! z)) || hayCruz (xs !! y) && not (celdaOcupada (xs !! z)) && not (celdaOcupada (xs !! x)) || hayCruz (xs !! z) && not (celdaOcupada (xs !! y)) && not (celdaOcupada (xs !! x)) = 10 + evaluaLineas1x xs xss
    | otherwise = evaluaLineas1x xs xss

evaluaLineas2o :: [Celda] -> [[Int]] -> Valor
evaluaLineas2o xs [] = 0
evaluaLineas2o xs ([x,y,z]:xss)
    | hayCirculo (xs !! x) && hayCirculo (xs !! y) && not (celdaOcupada (xs !! z)) || hayCirculo (xs !! x) && hayCirculo (xs !! z) && not (celdaOcupada (xs !! y)) || hayCirculo (xs !! z) && hayCirculo (xs !! y) && not (celdaOcupada (xs !! x)) = 100 + evaluaLineas2o xs xss
    | otherwise = evaluaLineas2o xs xss

evaluaLineas1o :: [Celda] -> [[Int]] -> Valor
evaluaLineas1o xs [] = 0
evaluaLineas1o xs ([x,y,z]:xss)
    | hayCirculo (xs !! x) && not (celdaOcupada (xs !! y)) && not (celdaOcupada (xs !! x)) || hayCirculo (xs !! y) && not (celdaOcupada (xs !! z)) && not (celdaOcupada (xs !! x)) || hayCirculo (xs !! z) && not (celdaOcupada (xs !! y)) && not (celdaOcupada (xs !! x)) = 10 + evaluaLineas1o xs xss
    | otherwise = evaluaLineas1o xs xss

evalua :: Tablero -> Valor
evalua (Tab (b, xs))
    | ganaCruz (Tab (b, xs)) = -1000000
    | ganaCirculo (Tab (b, xs)) = 1000000
    | otherwise = -((evaluaLineas2x xs ys + evaluaLineas1x xs ys) - (evaluaLineas2o xs ys + evaluaLineas1o xs ys))
    where ys = indicesLineas3EnRaya xs

    
minimax :: Profundidad -> (Tablero -> [Tablero]) -> (Tablero -> Valor) -> ([Valor] -> Valor) -> ([Valor] -> Valor) -> Tablero -> Valor
minimax prof tablerosSiguientes evalua peor mejor problema
    | prof == 0 || null siguientes = evalua problema
    | otherwise = mejor (map (minimax (prof -1) tablerosSiguientes evalua mejor peor) siguientes)
    where siguientes = tablerosSiguientes problema


minimaxMain prof tablerosSiguientes evalua problema
    | null siguientes = error "Fin del juego"
    | otherwise =  fst (maximum' sigsVals)
    where siguientes = tablerosSiguientes problema
          valoraciones = map (minimax (prof - 1) tablerosSiguientes evalua maximum minimum) siguientes
          sigsVals = zip siguientes valoraciones  
          maximum' = foldr1 max' 
          max' (t1, v1) (t2, v2)
            | v1 <= v2 = (t2, v2)
            | otherwise = (t1, v1)

main :: IO ()
main = do 
       hSetBuffering stdout NoBuffering 
       putStrLn "TRES EN RAYA"
       putStrLn "Nuevo juego? si/no (recupera partida anterior): "
       s <- getLine
       if head s `elem` "s"
          then do
               putStrLn "Comienza el juego? humano/ordenador: "
               l <- getLine
               putStrLn "Nivel de dificultad del 1 al 9: "
               p <- getLine
               if head l `elem` "h"
                  then do 
                       humano (read p :: Profundidad) (tableroInicial True)
                  else  do
                        ordenador (read p :: Profundidad) (tableroInicial False)
          else do
                (Tab (b,xs), p) <- recuperarPartida
                if b
                  then humano p (Tab (b,xs))
                  else ordenador p (Tab (b,xs))
               

humano :: Profundidad -> Tablero -> IO ()
humano p t = do 
             putStrLn (mostrarTablero t)
             putStr "Indica el lugar donde colocar la ficha: "
             l <- getLine
             let t' = ponerFicha t (read l :: Posicion)
             if hayGanador t'
             then do
                  putStrLn "Gana el humano. "
                  putStrLn (mostrarTablero2 t')
                else if tableroCompleto t'
                    then do
                         putStrLn "Empate. "
                         putStrLn (mostrarTablero2 t')
                    else do putStr "Quieres seguir jugando? si/no: "
                            r <- getLine
                            if head r `elem` "s" 
                                then ordenador p t'
                                else guardarPartida p t'
                               
guardarPartida :: Profundidad -> Tablero -> IO ()
guardarPartida p (Tab (b, xs)) = do
                                 writeFile "PartidaPendiente.txt" ((show (Tab (b, xs))) ++ "\n" ++ (show p))
                                                           
recuperarPartida :: IO (Tablero, Profundidad)
recuperarPartida = do
                   contenido <- readFile "PartidaPendiente.txt"
                   let [lineaTa,lineaProf] = lines contenido
                   return (read lineaTa,read lineaProf)
                   

ordenador :: Profundidad -> Tablero -> IO ()
ordenador p t = do 
                putStrLn (mostrarTablero t)
                putStrLn "\nJugada: "
                let t' = minimaxMain p tablerosSiguientes evalua t
                if hayGanador t'
                then do
                     putStrLn "Gana la maquina. "
                     putStrLn (mostrarTablero2 t')
                    else if tableroCompleto t'
                        then do
                             putStrLn "Empate. "
                             putStrLn (mostrarTablero2 t')
                        else humano p t'
