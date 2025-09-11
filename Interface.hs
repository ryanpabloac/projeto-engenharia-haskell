module Interface (hubMenus) where

import Calculus
import Geometry
import Types
import Algebra 
import DataStructures
import Algorithms 

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
      "algoritmos" -> menuAlgoritmos
      "estruturas_de_dados" -> menuEstruturasDeDados


-- |Menus Específicos
menuPrincipal :: IO ()
menuPrincipal = do
    exibirOpções listaOp
    op <- lerOpção opMin opMax
    
    case op of
      0 -> do putStrLn "Programa encerrado!"
              return ()
      3 -> do hubMenus "calculo"
      4 -> do hubMenus "geometria"
      5 -> do hubMenus "algoritmos"
      6 -> do hubMenus "estruturas_de_dados"
      
    where
      listaOp = [
        "[3] Cálculo Diferencial e Integral",
        "[4] Geometria",
        "[5] Algoritmos",
        "[6] Estruturas de Dados",
        "[0] Sair"]
      opMin = 0
      opMax = 6 --mudar
      
{-
    "[6] Executar todos os testes", -- trocar a opção
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

-- MENU ALGEBRA LINEAR 

menuAlgebra :: IO () 
menuAlgebra = do
    exibirOpções listaOp
    op <- lerOpção opMin opMax
    f <- menuFunção
    
    case op of
        0 -> menuPrincipal
        1 -> do
            putStr "Forneça a primeira matriz: "
            m1 <- readLn
            putStr "Agora, forneça a segunda: "
            m2 <- readLn
            let resultado = somarMatrizes m1 m2
            putStrLn ("A soma é: " ++ show resultado)
            menuAlgebra
        2 -> do
            putStr "Forneça a primeira matriz: "
            m1 <- readLn
            putStr "Forneça a segunda matriz: "
            m2 <- readLn
            let resultado = multiplicarMatrizes m1 m2
            putStrLn ("O produto é: " ++ show resultado)
            menuAlgebra
        3-> do
            putStr "Digite a matriz a ser transposta: "
            m <- readLn
            let resultado = transpostaMatriz m
            putStrLn ("A transposta é: " ++ show resultado)
            menuAlgebra
        4-> do
            putStr "O determinante será calculado. Forneça a matriz: "
            m <- readLn
            let resultado = determinante m
            putStrLn ("O determinante é: " ++ show resultado)
            menuAlgebra
        5-> do
            putStr "O sistema linear será resolvido. Digite a matriz: "
            m1 <- readLn
            putStr "Agora, digite o vetor: "
            v2 <- readLn
            let resultado = resolverSistemaLinear m1 v2
            putStrLn ("O resultado é: " ++ show resultado)
            menuAlgebra
        6 -> do
            putStr "Digite o primeiro vetor: "
            v1 <- readLn
            putStr "Digite o segundo vetor: "
            v2 <- readLn
            let resultado = produtoEscalar v1 v2
            putStrLn ("O produto escalar dos vetores é: " ++ show resultado)
            menuAlgebra
        7 -> do
            putStr "Informe o vetor para o cálculo da norma: "
            v <- readLn
            let resultado = normaVetor v
            putStrLn ("A norma do vetor é: " ++ show resultado)
            menuAlgebra
        8 -> do
            putStr "Informe o primeiro vetor: "
            v1 <- readLn
            putStr "Informe o segundo: "
            v2 <- readLn
            let resultado = anguloEntreVetores v1 v2
            putStrLn ("O ângulo entre os vetores é: " ++ show resultado)
            menuAlgebra
            
    where
        listaOp = [
           "[1] Somar matrizes.",
           "[2] Multiplicar matrizes.",
           "[3] Transpor matrizes.",
           "[4] Calcular o determinante.",
           "[5] Resolver um sistema linear.",
           "[6] Produto escalar de vetores.",
           "[7] Calcular a norma de um vetor.",
           "[8] Calcular o ângulo entre vetores.",
           "[0] Voltar."
        ]
        
        opMin = 0
        opMax = 8

-- |Menu de Algoritmos
menuAlgoritmos :: IO ()
menuAlgoritmos = do
    exibirOpções listaOp
    op <- lerOpção opMin opMax

    1 -> do
        putStrLn "Realizando Merge Sort..."
        putStr "Entre com a lista (ex: [3,1,2]): "
        l <- readLn :: IO [Int]
        let resultado = mergeSort l
        putStrLn $ "Resultado: " ++ (show resultado)
        menuAlgoritmos
      2 -> do
        putStrLn "Realizando Quick Sort..."
        putStr "Entre com a lista (ex: [3,1,2]): "
        l <- readLn :: IO [Int]
        let resultado = quickSort l
        putStrLn $ "Resultado: " ++ (show resultado)
        menuAlgoritmos
      3 -> do
        putStrLn "Realizando Insertion Sort..."
        putStr "Entre com a lista (ex: [3,1,2]): "
        l <- readLn :: IO [Int]
        let resultado = insertionSort l
        putStrLn $ "Resultado: " ++ (show resultado)
        menuAlgoritmos
      4 -> do
        putStrLn "Inserindo um elemento em uma lista ordenada..."
        putStr "Entre com a lista ordenada (ex: [1,3,5]): "
        l <- readLn :: IO [Int]
        putStr "Entre com o elemento a ser inserido: "
        v <- readLn :: IO Int
        let resultado = inserirOrdenado v l
        putStrLn $ "Resultado: " ++ (show resultado)
        menuAlgoritmos
      0 -> do
        putStrLn "Retornando ao menu principal."
        hubMenus "principal"

    where
      listaOp = [
          "[1] Busca Binária",
          "[2] Bubble Sort",
          "[0] Retornar"
        ]
      opMin = 0
      opMax = 2

-- |Menu de Estruturas de Dados
menuEstruturasDeDados :: IO ()
menuEstruturasDeDados = do
    exibirOpções listaOp
    op <- lerOpção opMin opMax

    case op of
      
      1 -> do
        putStrLn "Construindo uma árvore de busca binária..."
        putStr "Entre com a lista de elementos para a árvore (ex: [5, 3, 7, 2, 4, 6, 8]): "
        l <- readLn :: IO [Int]
        let arvore = construirArvore l
        putStrLn $ "Árvore construída: " ++ (show arvore)
        menuEstruturasDeDados
      2 -> do
        putStrLn "Buscando um valor na árvore..."
        putStr "Primeiro, construa a árvore. Entre com a lista de elementos: "
        l <- readLn :: IO [Int]
        let arvore = construirArvore l
        putStr "Agora, entre com o valor a ser buscado: "
        v <- readLn :: IO Int
        let resultado = buscarArvore v arvore
        case resultado of
            Just val -> putStrLn $ "Valor encontrado: " ++ show val
            Nothing  -> putStrLn "Valor não encontrado."
        menuEstruturasDeDados
      0 -> do
        putStrLn "Retornando ao menu principal."
        hubMenus "principal"

    where
      listaOp = [
          "[1] Construir Árvore",
          "[2] Buscar na Árvore",
          "[0] Retornar"
        ]
      opMin = 0
      opMax = 3
