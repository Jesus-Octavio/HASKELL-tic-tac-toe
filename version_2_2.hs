-- ELISA CASADO GÓMEZ. JESÚS OCTAVIO RABOSO.

-- Este es el código necesario para jugar al 3 en raya. Empezamos importando los módulos que necesitamos para trabajar con listas
-- y con entrada salida. Después, definimos 6 tipos de datos con data o con type dependiendo de lo que necesitamos.

import Data.List
import System.IO

-- El tipo tablero lo definimos como un booleano que indica de quién es el turno (True para el humano y False para el ordenador) y
-- una lista de celdas.

data Tablero =  Tab (Bool, [Celda])
    deriving(Show, Eq, Read)

-- El tipo Celda tiene dos constructoras, P si la casilla está vacía y F si la casilla está ocupada por una ficha.

data Celda = P Posicion | F Ficha
    deriving(Show, Eq, Read)
    
-- El tipo Ficha puede ser un Circulo o una Cruz dependiendo de quién es el turno, de esta forma, Circulo es siempre del ordenador y
-- Cruz, del humano.

data Ficha = Circulo | Cruz
    deriving(Show, Eq, Read)
    
-- Los tres siguientes tipos son un entero para denotar una posición (que está vacía), la profundidad con que se recorre un árbol y 
-- el valor que denota lo bueno o malo que es un tablero.     
    
type Posicion = Int

type Profundidad = Int

type Valor = Int

-- Comenzamos creando una función que devuelva un tablero inicial, vacío, formado por el booleano  dado que indica de quién es el
-- turno y la lista de celdas vacías.

tableroInicial :: Bool -> Tablero
tableroInicial b
    | b = Tab (True, [P 0,P 1,P 2,P 3,P 4,P 5,P 6,P 7,P 8])
    | otherwise = Tab (False, [P 0,P 1,P 2,P 3,P 4,P 5,P 6,P 7,P 8])
    
-- La siguiente función devuelve un booleano que indica si el tablero está completo o no, es decir, si todas las celdas tienen 
-- una ficha o no.

tableroCompleto :: Tablero -> Bool
tableroCompleto (Tab (b, xs)) = and (map celdaOcupada xs)

-- Comprobamos si un tablero está bien definido, es decir, que está formado por nueve celdas.

esTablero :: Tablero -> Bool    
esTablero (Tab (b, xs)) = length xs == 9

-- Las siguientes seis funciones las creamos para mostrar el tablero al usuario. La primera para simbolizar la celda como el número
-- de posición si esta está vacía, como O si hay un círculo o como X si hay una cruz. La segunda muestra el tablero como una lista
-- de cadenas de caracteres en una única línea. La tercera lo muestra como una cadena de caracteres en una única línea. La cuarta
-- muestra, dado un número del 1 al 3, la fila que corresponde. La quinta muestra el tablero completo, además de una frase que
-- indica de quién es el turno. Por último, la sexta función hace lo mismo que la anterior pero omitiendo de quién es el turno.

mostrarCelda :: Celda -> [Char]
mostrarCelda (P n) = show n
mostrarCelda (F Circulo) = "O"
mostrarCelda (F Cruz) = "X"

mostrarTableroLista :: Tablero -> [String]
mostrarTableroLista (Tab (b, xs))
    | esTablero (Tab (b, xs)) = map mostrarCelda xs
    | otherwise = error "No es tablero"

mostrarTableroCadena :: Tablero -> String
mostrarTableroCadena t = filter (/=' ') (unwords (mostrarTableroLista t))

mostrarLinea :: Int -> Tablero -> String
mostrarLinea n t
    | n == 1 = intersperse '|' (take 3 (mostrarTableroCadena t))
    | n == 2 = intersperse '|' (take 3 (drop 3 (mostrarTableroCadena t)))
    | n == 3 = intersperse '|' (drop 6 (mostrarTableroCadena t))
    | otherwise = error "Solo hay tres lineas"
    
mostrarTablero :: Tablero -> String
mostrarTablero (Tab (b, xs))
    | b =  resto ++ "\n" ++ "Es el turno de HUMANO (CRUZ)"
    | otherwise = resto ++ "\n" ++ "Es el turno de ORDENADOR (CIRCULO)"
    where resto = linea1 ++ "\n-----\n" ++ linea2 ++ "\n-----\n" ++ linea3
          linea1 = mostrarLinea 1 (Tab (b, xs))
          linea2 = mostrarLinea 2 (Tab (b, xs))
          linea3 = mostrarLinea 3 (Tab (b, xs))

mostrarTablero2 :: Tablero -> String
mostrarTablero2 t = linea1 ++ "\n-----\n" ++ linea2 ++ "\n-----\n" ++ linea3
    where linea1 = mostrarLinea 1 t
          linea2 = mostrarLinea 2 t
          linea3 = mostrarLinea 3 t
          
-- La siguiente función, dada una lista de celdas (lo damos así y no como tablero pues en la siguiente necesitaremos que la entrada
-- sea una lista) y una tupla con una posición y una celda, cambia el estado de la celda de la posición dada por la nueva celda dada.

cambiar :: [Celda] -> (Posicion, Celda) -> [Celda]
cambiar xs (i, e) = antes ++ [e] ++ despues
    where (antes, _:despues) = splitAt i xs
    
-- Ahora creamos dos funciones que, dado un tablero y una posición libre (pues en otro caso daría error), cambia el estado de la
-- posición de vacía a Cruz o Circulo según corresponda. 

ponerCruz :: Tablero -> Posicion -> Tablero
ponerCruz (Tab (b,xs))  p
    | not (esTablero (Tab (b,xs))) = error "No es tablero"
    | celdaOcupada (devuelveCelda p (Tab (b, xs)))= error "Movimiento invalido"
    | otherwise = Tab (False, cambiar xs (p, (F Cruz)))

ponerCirculo :: Tablero -> Posicion -> Tablero
ponerCirculo (Tab (b, xs))  p
    | not (esTablero (Tab (b,xs))) = error "No es tablero"
    | celdaOcupada(devuelveCelda p (Tab (b, xs)))= error "Movimiento invalido"
    | otherwise = Tab (True, cambiar xs (p, (F Circulo)))

-- Esta función utiliza las dos anteriores para cambiar el estado de una celda dependiendo del turno que hay en el tablero.

ponerFicha :: Tablero -> Posicion -> Tablero
ponerFicha (Tab (b, xs)) p
    | b == True = ponerCruz (Tab (b, xs)) p
    | otherwise = ponerCirculo (Tab (b,xs)) p
    
-- La función siguiente, dado un entero y un tablero, devuelve la celda que indica el entero dado.    

devuelveCelda :: Int -> Tablero -> Celda
devuelveCelda n (Tab (b, xs))
    | not (esTablero (Tab (b, xs))) = error "No es tablero"
    | (n < 0) || (n > 8) = error "Celda inexistente"
    | otherwise = xs !! n

-- La siguiente función, dada una celda, devuelve si está ocupada o libre. 

celdaOcupada :: Celda -> Bool
celdaOcupada (P _) = False
celdaOcupada (F _) = True

-- Esta función, una de las necesarias para el minimax, dado un tablero y teniendo en cuenta el turno, devuelve la lista de tableros
-- posibles a partir del dado.

tablerosSiguientes :: Tablero -> [Tablero]
tablerosSiguientes (Tab (b, xs))
    | tableroCompleto (Tab (b, xs)) && hayEmpate (Tab (b, xs)) = []
    | ganaCruz (Tab (b, xs)) =  []
    | ganaCirculo (Tab (b, xs)) =  []
    | b = map (ponerCruz (Tab (b, xs))) [x | x <- map posicionesCeldasLibres xs, x >= 0]
    | otherwise = map (ponerCirculo (Tab (b, xs))) [x | x <- map posicionesCeldasLibres xs, x >= 0]
      
-- La función siguiente devolverá, dada una celda, su posición si está vacía o -1 si está ocupada (por una ficha)      
      
posicionesCeldasLibres :: Celda -> Posicion
posicionesCeldasLibres (P x) = x
posicionesCeldasLibres (F x) = -1

-- Las dos funciones que aparecen a continuación devuelven una lista de las posiciones que tienen una determinada ficha.

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

-- Dadas dos listas de posiciones, comprobamos si la primera contiene completamente a la segunda, es decir, si todos los elementos 
-- de la segunda lista están en la primera.

subconjunto :: [Posicion] -> [Posicion] -> Bool
subconjunto ys xs = all (`elem` ys) xs

-- Esta función la utilizaremos como auxiliar, dada una lista de celdas, devuelve una lista de ocho listas de enteros de forma
-- que las tres primeras corresponden a los índices asociados a las filas, las tres siguientes, a los asociados a las columnas
-- y, las dos últimas, a los asociados a las diagonales. Por ejemplo:
-- >> indicesLineas3EnRaya [P 0, F Cruz, P 2, P 3, F Circulo, P 5, P 6, P 7, P 8]
-- >> [[0,1,2],[3,4,5],[6,7,8],[0,3,6],[1,4,7],[2,5,8],[0,4,8],[2,4,6]]

indicesLineas3EnRaya :: [Celda] -> [[Int]]
indicesLineas3EnRaya xs = filas ++ columnas ++ diagonales
        where n = round (sqrt (fromIntegral (length xs)))
         
              columnas =  [[col + i * n | i <- [0..n - 1] ] | col <- [0..n - 1]]
              diagonales = [[(i `mod` n) + i * n | i <- [0..n - 1]], [i + n - 1 | i <- [0, n - 1..(n - 1) * (n - 1)]]]

-- Las dos funciones que aparecen a continuación indican si, dado un tablero, gana el humano (la primera) 
-- o gana el ordenador (la segunda)

ganaCruz :: Tablero -> Bool
ganaCruz (Tab (b, xs)) = or (map (subconjunto cruces) [x | x <- indicesLineas3EnRaya xs])
                where cruces = posicionesConCruz (Tab (b,xs))

ganaCirculo :: Tablero -> Bool
ganaCirculo (Tab (b, xs)) = or (map (subconjunto circulos) [x | x <- indicesLineas3EnRaya xs])
                where circulos = posicionesConCirculo (Tab (b,xs))

-- Las dos funciones siguientes indican si, dado un tablero, hay un ganador (la primera) ó hay un empate (la segunda)

hayGanador:: Tablero -> Bool
hayGanador t = ganaCirculo t || ganaCruz t

hayEmpate :: Tablero -> Bool
hayEmpate t = not (hayGanador t)

-- Estas dos funciones serán necesarias para el minimax y, dado una lista de valores procedentes de tableros, devuelven, 
-- respectivamente, el mejor (el máximo) y el peor (el mínimo) valor.

mejor :: [Valor] -> Valor
mejor xs = maximum xs

peor :: [Valor] -> Valor
peor xs = minimum xs

-- Las dos funciones siguientes, dada una celda, devuelven, respectivamente, si está ocupada por una cruz y si está ocupada
-- por un círculo.

hayCruz :: Celda -> Bool
hayCruz (F Cruz) = True
hayCruz _ = False

hayCirculo :: Celda -> Bool
hayCirculo (F Circulo) = True
hayCirculo _ = False

-- Las cuatro funciones siguientes son auxiliares para la que está a continuación de ellas. La primera evalúa las líneas (filas,
-- columnas y diagonales) con dos cruces (100 por cada una); la segunda, las líneas con una cruz (10 por cada una); la tercera, 
-- las líneas con dos círculos (100 por cada una); la cuarta, las líneas con un círculo (10 por cada una). Por último, la quinta
-- función necesaria para el minimax devuelve, dado un tablero, su valor total, teniendo en cuenta que si gana el humano vale
-- -1000000 y si gana el ordenador, 1000000.

evaluaLineas2x :: [Celda] -> [[Int]] -> Valor
evaluaLineas2x xs [] = 0
evaluaLineas2x xs ([x,y,z]:xss)
    | xxb || xbx || bxx = 100 + evaluaLineas2x xs xss
    | otherwise = evaluaLineas2x xs xss
    where xxb = hayCruz (xs !! x) && hayCruz (xs !! y) && not (celdaOcupada (xs !! z))
          xbx = hayCruz (xs !! x) && hayCruz (xs !! z) && not (celdaOcupada (xs !! y))
          bxx = hayCruz (xs !! z) && hayCruz (xs !! y) && not (celdaOcupada (xs !! x))

evaluaLineas1x :: [Celda] -> [[Int]] -> Valor
evaluaLineas1x xs [] = 0
evaluaLineas1x xs ([x,y,z]:xss)
    | xbb || bxb || bbx = 10 + evaluaLineas1x xs xss
    | otherwise = evaluaLineas1x xs xss
    where xbb = hayCruz (xs !! x) && not (celdaOcupada (xs !! y)) && not (celdaOcupada (xs !! z))
          bxb = hayCruz (xs !! y) && not (celdaOcupada (xs !! z)) && not (celdaOcupada (xs !! x))
          bbx = hayCruz (xs !! z) && not (celdaOcupada (xs !! y)) && not (celdaOcupada (xs !! x))

evaluaLineas2o :: [Celda] -> [[Int]] -> Valor
evaluaLineas2o xs [] = 0
evaluaLineas2o xs ([x,y,z]:xss)
    | oob || obo || boo = 100 + evaluaLineas2o xs xss
    | otherwise = evaluaLineas2o xs xss
    where oob = hayCirculo (xs !! x) && hayCirculo (xs !! y) && not (celdaOcupada (xs !! z))
          obo = hayCirculo (xs !! x) && hayCirculo (xs !! z) && not (celdaOcupada (xs !! y))
          boo = hayCirculo (xs !! z) && hayCirculo (xs !! y) && not (celdaOcupada (xs !! x))

evaluaLineas1o :: [Celda] -> [[Int]] -> Valor
evaluaLineas1o xs [] = 0
evaluaLineas1o xs ([x,y,z]:xss)
    | obb || bob || bbo = 10 + evaluaLineas1o xs xss
    | otherwise = evaluaLineas1o xs xss
    where obb = hayCirculo (xs !! x) && not (celdaOcupada (xs !! y)) && not (celdaOcupada (xs !! x))
          bob = hayCirculo (xs !! y) && not (celdaOcupada (xs !! z)) && not (celdaOcupada (xs !! x))
          bbo = hayCirculo (xs !! z) && not (celdaOcupada (xs !! y)) && not (celdaOcupada (xs !! x)) 

evalua :: Tablero -> Valor
evalua (Tab (b, xs))
    | ganaCruz (Tab (b, xs)) = -1000000
    | ganaCirculo (Tab (b, xs)) = 1000000
    | otherwise = -(cruces - circulos)
    where ys = indicesLineas3EnRaya xs
          cruces = evaluaLineas2x xs ys + evaluaLineas1x xs ys
          circulos = evaluaLineas2o xs ys + evaluaLineas1o xs ys

-- Esta función, tal como vimos en clase, recorre el árbol de los posibles tableros y devuelve el valor de la mejor opción.
    
minimax :: Profundidad -> (Tablero -> [Tablero]) -> (Tablero -> Valor) -> ([Valor] -> Valor) -> ([Valor] -> Valor) -> Tablero -> Valor
minimax prof tablerosSiguientes evalua peor mejor problema
    | prof == 0 || null siguientes = evalua problema
    | otherwise = mejor (map (minimax (prof -1) tablerosSiguientes evalua mejor peor) siguientes)
    where siguientes = tablerosSiguientes problema

-- Esta función, también vista en clase, da error si el juego ha terminado o devuelve el tablero cuyo valor es el mejor.

minimaxMain :: Profundidad -> (Tablero -> [Tablero]) -> (Tablero -> Valor) -> Tablero -> Tablero
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

-- Finalmente, esta función es la que controla la interfaz con el usuario. Este ha de empezar escribiendo main, se le preguntará si
-- desea empezar un nuevo juego o cargar una partida que tenga guardada. Si es nueva, tendrá que decidir quién empieza el juego y el
-- nivel de dificultad de este. Con la función humano, se le preguntará en que posición desea poner la ficha y si quiere seguir
-- jugando. La función ordenador regula el juego que hace el contrincante. La función para guardar la partida crea un fichero llamado
-- PartidaPendiente.txt y escribe el tablero en la primera línea. La función para recuperar la partida lee la primera línea del 
-- fichero PartidaPendiente.txt y reanuda el juego.

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
                  putStrLn "Gana el HUMANO. "
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
                     putStrLn "Gana el ORDENADOR. "
                     putStrLn (mostrarTablero2 t')
                    else if tableroCompleto t'
                        then do
                             putStrLn "Empate. "
                             putStrLn (mostrarTablero2 t')
                        else humano p t'
