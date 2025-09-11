module Interface (hubMenus) where

import Calculus
import Geometry
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
      
   where
      listaOp = [
       "[3] Cálculo Diferencial e Integral",
       "[0] Sair"]
      opMin = 0
      opMax = 6
      
 {-
    "[6] Executar todos os testes",
 -}
menuCálculo :: IO ()
menuCálculo = do
   exibirOpções listaOp
   op <- lerOpção opMin opMax
   f <- menuFunção
   
   case op of
      0 -> do menuPrincipal
      1 -> do
         putStr "Entre com um valor x: "
         x <- readLn
         let resultado = avaliarFuncao f x
         putStrLn $ "Resultado: " ++ (show resultado)
         menuCálculo
         
      2 -> do
         putStr "Entre com um valor x: "
         x <- readLn
         let resultado = derivadaNumerica f x
         putStrLn $ "Resultado: " ++ (show resultado)
         menuCálculo
         
      3 -> do
         putStr "Limite inferior: "
         a <- readLn
         putStr "Limite superior: "
         b <- readLn
         putStr "Quantidade de somas: "
         n <- readLn
         let resultado = integralNumerica f a b n 
         putStrLn $ "Resultado: " ++ (show resultado)
         menuCálculo
         
      4 -> do
         putStr "Limite inferior: "
         a <- readLn
         putStr "Limite superior: "
         b <- readLn
         let resultado = encontrarRaizes f a b
         putStrLn $ "Resultado: " ++ (show resultado)
         menuCálculo
      
      5 -> do
         putStr "Limite inferior: "
         a <- readLn
         putStr "Limite superior: "
         b <- readLn
         let resultado = pontosCriticos f a b
         putStrLn $ "Resultado: " ++ (show resultado)
         menuCálculo
         
      6 -> do
         putStr "Limite inferior: "
         a <- readLn
         putStr "Limite superior: "
         b <- readLn
         let resultado = encontrarMinimo f a b
         putStrLn $ "Resultado: " ++ (show resultado)
         menuCálculo
         
      7 -> do
         putStr "Limite inferior: "
         a <- readLn
         putStr "Limite superior: "
         b <- readLn
         let resultado = encontrarMaximo f a b
         putStrLn $ "Resultado: " ++ (show resultado)
         menuCálculo
           
   where
      listaOp = [
       "[1] Avaliar Função",
       "[2] Derivada",
       "[3] Integral",
       "[4] Encontrar Raízes",
       "[5] Pontos Críticos",
       "[6] Encontrar Mínimo",
       "[7] Encontrar Máximo",
       "[0] Voltar"
       ]
      opMin = 0
      opMax = 7
 
 
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
      
menuGeometria :: IO ()
menuGeometria = do
    exibirOpções listaOp
    op <- lerOpção opMin opMax

    case op of
      1 -> do
          putStrLn "Calculando a distância entre dois pontos 2D..."
          -- Código para coletar pontos e chamar a função distanciaEntrePontos
      2 -> do
          putStrLn "Calculando a distância entre dois pontos 3D..."
          -- Código para coletar pontos e chamar a função distancia3D
      3 -> do
          putStrLn "Calculando o ponto médio entre dois pontos 2D..."
          -- Código para coletar pontos e chamar a função pontoMedio
      4 -> do
          putStrLn "Calculando a área de uma figura..."
          -- Código para coletar a figura e chamar a função calcularArea
      5 -> do
          putStrLn "Calculando o perímetro de uma figura..."
          -- Código para coletar a figura e chamar a função calcularPerimetro
      6 -> do
          putStrLn "Calculando o volume de uma figura..."
          -- Código para coletar a figura e chamar a função calcularVolume
      7 -> do
          putStrLn "Encontrando a interseção entre duas retas..."
          -- Código para coletar as retas e chamar a função intersecaoRetas
      0 -> do
          putStrLn "Retornando ao menu principal."
          hubMenus "principal"

  where
    listaOp = [
        "[1] Distância entre pontos 2D",
        "[2] Distância entre pontos 3D",
        "[3] Ponto Médio (2D)",
        "[4] Calcular Área",
        "[5] Calcular Perímetro",
        "[6] Calcular Volume",
        "[7] Interseção entre retas",
        "[0] Retornar"]
    opMin = 0
    opMax = 7
    
lerPonto2D :: IO Ponto2D
lerPonto2D = do
   putStr "\nEntre com a coordenada x: "
   x <- readLn
   putStr "Entre com a coordenada y: "
   y <- readLn
   return (Ponto2D x y)
      
      
