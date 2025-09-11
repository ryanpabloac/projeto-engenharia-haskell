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
      "geometria" -> menuGeometria


-- |Menus Específicos
menuPrincipal :: IO ()
menuPrincipal = do
   exibirOpções listaOp
   op <- lerOpção opMin opMax
   
   case op of
      0 -> do putStrLn "Programa encerrado!"
              return ()
      1 -> do hubMenus "geometria"
      3 -> do hubMenus "calculo"
      
   where
      listaOp = [
       "[1] Geometria Analítica",
       "[3] Cálculo Diferencial e Integral",
       "[0] Sair"]
      opMin = 0
      opMax = 6

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
          putStrLn "Calculando distância 2D"
          ponto0 <- lerPonto2D
          ponto1 <- lerPonto2D
          let resultado = distanciaEntrePontos ponto0 ponto1
          putStrLn $ "Resultado: " ++ (show resultado)
          hubMenus "geometria"
          
      2 -> do
          putStrLn "Calculando distância 3D"
          ponto0 <- lerPonto3D
          ponto1 <- lerPonto3D
          let resultado = distancia3D ponto0 ponto1
          putStrLn $ "Resultado: " ++ (show resultado)
          hubMenus "geometria"
          
      3 -> do
          putStrLn "Calculando ponto médio"
          ponto0 <- lerPonto2D
          ponto1 <- lerPonto2D
          let resultado = pontoMedio ponto0 ponto1
          putStrLn $ "Resultado: " ++ (show resultado)
          hubMenus "geometria"
          
      4 -> do
          putStrLn "Calculando a área"
          fig <- lerFigura
          let resultado = calcularArea fig
          putStrLn $ "Resultado: " ++ (show resultado)
          hubMenus "geometria"
          
      5 -> do
          putStrLn "Calculando o perímetro"
          fig <- lerFigura
          let resultado = calcularPerimetro fig
          putStrLn $ "Resultado: " ++ (show resultado)
          hubMenus "geometria"
          
      6 -> do
          putStrLn "Calculando o volume"
          fig <- lerFigura
          let resultado = calcularVolume fig
          putStrLn $ "Resultado: " ++ (show resultado)
          hubMenus "geometria"
          
      7 -> do
          putStrLn "Encontrando a interseção entre duas retas"
          putStrLn "Coordenadas da reta 1"
          p0 <- lerPonto2D
          p1 <- lerPonto2D
          putStrLn "\nCoordenadas da reta 2"
          p2 <- lerPonto2D
          p3 <- lerPonto2D
          let resultado = intersecaoRetas (p0,p1) (p2,p3)
          putStrLn $ "Resultado: " ++ (show resultado)
          hubMenus "geometria"
          
      0 -> do
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
   putStr "Entre com a coordenada x: "
   x <- readLn
   putStr "Entre com a coordenada y: "
   y <- readLn
   return (Ponto2D x y)
   
lerPontos2D :: IO [Ponto2D]
lerPontos2D = do
    putStrLn "Entre com um Ponto 2D (x y) ou pressione Enter para parar:"
    line <- getLine
    if null line
        then return []
        else do
            let coords = map read (words line) :: [Double]
            if length coords == 2
                then do
                    let ponto = Ponto2D (head coords) (last coords)
                    resto <- lerPontos2D
                    return (ponto : resto)
                else do
                    putStrLn "Entrada inválida. Por favor, entre com dois números."
                    lerPontos2D
   
lerPonto3D :: IO Ponto3D
lerPonto3D = do
   putStr "\nEntre com a coordenada x: "
   x <- readLn
   putStr "Entre com a coordenada y: "
   y <- readLn
   putStr "Entre com a coordenada z: "
   z <- readLn
   return (Ponto3D x y z)
   
lerFigura :: IO Figura
lerFigura = do
   exibirOpções listaOp
   op <- lerOpção opMin opMax

   case op of
      1 -> do
         putStr "Entre com a largura: "
         l <- readLn
         putStr "Entre com a altura: "
         a <- readLn
         return (Retangulo l a)
          
      2 -> do
         putStr "Entre com o raio: "
         r <- readLn
         return (Circulo r)
          
      3 -> do
         p0 <- lerPonto2D
         p1 <- lerPonto2D
         p2 <- lerPonto2D
         return (Triangulo p0 p1 p2)
          
      4 -> do
          pontos <- lerPontos2D
          return (Poligono pontos)
          
      5 -> do
         putStr "Entre com o raio: "
         r <- readLn
         return (Esfera r)
      6 -> do
         putStr "Entre com a largura: "
         l <- readLn
         putStr "Entre com a altura: "
         a <- readLn
         putStr "Entre com a profundidade: "
         p <- readLn
         return (Paralelepipedo l a p)
      7 -> do
         putStr "Entre com o raio: "
         r <- readLn
         putStr "Entre com a altura: "
         a <- readLn
         return (Cilindro r a)

  where
    listaOp = [
        "[1] Retângulo",
        "[2] Circulo",
        "[3] Triangulo",
        "[4] Poligono",
        "[5] Esfera",
        "[6] Paralelepipedo",
        "[7] Cilindro"]
    opMin = 0
    opMax = 7
   
 
      
      
