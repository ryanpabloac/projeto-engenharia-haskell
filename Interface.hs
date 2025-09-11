module Interface (hubMenus) where

import Calculus
import Types

-- |Funções Gerais
exibirOpções :: [String] -> IO ()
exibirOpções opções = do
   putStrLn ""
   putStrLn "==== MENU ===="
   putStrLn "Selecione uma opção válida: "
   putStrLn $ unlines opções


lerOpção :: Int -> Int -> IO Int
lerOpção min max = do
   putStr "|-> "
   opção <- readLn
   validar opção
   
   where
      validar op
            | op >= min && op <= max = return op
            | otherwise = do 
                            putStrLn "\nOpção inválida! Selecione outra válida."
                            lerOpção min max

hubMenus :: String -> IO ()
hubMenus menu = 
   case menu of
      "principal" -> menuPrincipal
      "calculo"   -> menuCálculo


-- |Menus Específicos
menuPrincipal :: IO ()
menuPrincipal = do
   exibirOpções listaOp
   op <- lerOpção opMin opMax
   
   case op of
      0 -> do putStrLn "Programa encerrado!"
              return ()
      3 -> do hubMenus "calculo"
      6 -> do hubMenus "testes"
      
   where
      listaOp = [
       "[3] Cálculo Diferencial e Integral",
       "[6] Executar testes",
       "[0] Sair"]
      opMin = 0
      opMax = 6
      
 
menuCálculo :: IO ()
menuCálculo = do
   exibirOpções listaOp
   op <- lerOpção opMin opMax
   
   case op of
      0 -> do menuPrincipal
      1 -> do
         f <- menuFunção
         putStr "Entre com um valor x: "
         x <- readLn
         let resultado = avaliarFuncao f x
         putStrLn $ "Resultado: " ++ (show resultado)
         menuCálculo
      2 -> do
         f <- menuFunção
         putStr "Entre com um valor x: "
         x <- readLn
         let resultado = derivadaNumerica f x
         putStrLn $ "Resultado: " ++ (show resultado)
         menuCálculo
         
   where
      listaOp = [
       "[1] Avaliar Função",
       "[2] Derivada",
       "[0] Voltar"
       ]
       {-
       "[3] Integral",
       "[4] Encontrar Raízes",
       "[5] Encontrar Máximo",
       "[6] Encontrar Mínimo",
       "[7] Comprimento da Curva",
       -}
      opMin = 0
      opMax = 7
 
 menuTestes :: IO ()
 menuTestes = do
    exibirOpções listaOp
    op <- lerOpção opMin opMax
    
    case op of
       0 -> do menuCálculo
       3 -> do 
    where
       opMin = 0
       opMax = 5
       listaOp = ["[3] Testes de Cálculo", "[0] Voltar"]
 
 
menuFunção :: IO Funcao
menuFunção = do
   exibirOpções listaOp
   op <- lerOpção opMin opMax
   
   case op of
      1 -> do
         putStr "Entre com o coeficiente a: "
         a <- readLn
         putStr "Entre com o coeficiente b: "
         b <- readLn
         putStr "Descreva a função aqui: "
         desc <- getLine
         return (Funcao (Linear a b) desc) -- ax + b
      2 -> do
         putStr "Entre com o coeficiente a: "
         a <- readLn
         putStr "Entre com o coeficiente b: "
         b <- readLn
         putStr "Entre com o coeficiente c: "
         c <- readLn
         putStr "Descreva a função aqui: "
         desc <- getLine
         return (Funcao (Quadratica a b c) desc) -- ax² + bx + c
      3 -> do
         putStr "Entre com o coeficiente a: "
         a <- readLn
         putStr "Entre com o coeficiente b: "
         b <- readLn
         putStr "Descreva a função aqui: "
         desc <- getLine
         return (Funcao (Exponencial a b) desc) -- a*e⁽bx⁾
      4 -> do
         putStr "Entre com o coeficiente a: "
         a <- readLn
         putStr "Entre com o coeficiente b: "
         b <- readLn
         putStr "Descreva a função aqui: "
         desc <- getLine
         return (Funcao (Exponencial a b) desc) -- a*ln(bx)
      5 -> do
         putStr "Qual função trigonométrica [Seno/Cosseno/Tangente]: "
         f <- readLn
         putStr "Entre com o coeficiente a: "
         a <- readLn
         putStr "Entre com o coeficiente b: "
         b <- readLn
         putStr "Descreva a função aqui: "
         desc <- getLine
         return (Funcao (Trigonometrica f a b) desc) -- a*func(bx)      
   where
      opMin = 0
      opMax = 5
      listaOp = ["[1] Linear", "[2] Quadrática", "[3] Exponencial", "[4] Logaritimica", "[5] Trigonometrica"]
      
      
