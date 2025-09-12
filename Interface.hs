module Interface (hubMenus) where

import Calculus
import Geometry
import Types
import Algebra
import DataStructures
import Algorithms 
import Engineering.Civil
import Engineering.Electrical
import Engineering.Mechanical

-- |Funções Gerais
exibirOpções :: [String] -> IO ()
exibirOpções opções = do
    putStrLn ""
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
      "algebra" -> menuAlgebra
      "engenharias" -> menuEngenharias
      "civil" -> menuCivil
      "eletrica" -> menuEletrica
      "mecanica" -> menuMecanica


-- |Menus Específicos
menuPrincipal :: IO ()
menuPrincipal = do
    exibirOpções listaOp
    op <- lerOpção opMin opMax
    
    case op of
      0 -> do putStrLn "Programa encerrado!"
              return ()
      1 -> do hubMenus "geometria"
      2 -> do hubMenus "algebra"
      3 -> do hubMenus "calculo"
      4 -> do hubMenus "algoritmos"
      5 -> do hubMenus "estruturas_de_dados"
      6 -> do hubMenus "engenharias"
      
    where
      listaOp = [
        "[1] Geometria Analítica",
        "[2] Álgebra Linear",
        "[3] Cálculo Diferencial e Integral",
        
        "[4] Algoritmos",
        "[5] Estruturas de Dados",
        "[6] Engenharias",
        "[0] Sair"]
      opMin = 0

      opMax = 6

menuCálculo :: IO ()
menuCálculo = do
    putStrLn "=== MENU CÁLCULO ==="
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
          putStr "Tamanho dos intervalos: "
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
    putStrLn "=== MENU FUNÇÕES ==="
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
    putStrLn "=== MENU GEOMETRIA ==="
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

-- MENU ALGEBRA LINEAR 

menuAlgebra :: IO () 
menuAlgebra = do
    putStrLn "=== MENU ÁLGEBRA ==="
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
    putStrLn "=== MENU ALGORITMOS ==="
    exibirOpções listaOp
    op <- lerOpção opMin opMax
    
    case op of
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
          "[1] Merge Sort",
          "[2] Quick Sort",
          "[3] Insertion Sort",
          "[4] Inserir Ordenado",
          "[0] Retornar"
        ]
      opMin = 0
      opMax = 4

-- |Menu de Estruturas de Dados
menuEstruturasDeDados :: IO ()
menuEstruturasDeDados = do
    putStrLn "=== MENU ESTRUTURA DE DADOS ==="
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
            True -> putStrLn $ "Valor encontrado: " ++ show v
            False -> putStrLn "Valor não encontrado."
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
      
menuEngenharias :: IO ()
menuEngenharias = do
   putStrLn "=== MENU ENGENHARIAS ==="
   exibirOpções listaOp
   op <- lerOpção opMin opMax
   
   case op of
      0 -> do return ()
      1 -> do hubMenus "civil"
      2 -> do hubMenus "mecanica"
      3 -> do hubMenus "eletrica"
   where
      listaOp = [
        "[1] Engenharia Civil",
        "[2] Engenharia Mecânica",
        "[3] Engenharia Elétrica",
        "[0] Voltar"]
      opMin = 0
      opMax = 3
      
menuCivil :: IO ()
menuCivil = do
   putStrLn "=== MENU ENGENHARIA CIVIL ==="
   exibirOpções listaOp
   op <- lerOpção opMin opMax
   
   case op of
      0 -> do return ()
      1 -> do
         putStr "Entre com a largura da base: "
         l <- readLn
         putStr "Entre com a altura da seção: "
         a <- readLn
         let resultado = momentoInerciaRetangular l a
         putStrLn $ "Resultado: " ++ (show resultado)
         hubMenus "civil"
         
      2 -> do
         putStr "Entre com a força aplicada: "
         f <- readLn
         putStr "Entre com a área da seção: "
         a <- readLn
         let resultado = tensaoNormal f a
         putStrLn $ "Resultado: " ++ (show resultado)
         hubMenus "civil"
         
      3 -> do
         putStr "Entre com a carga: "
         c <- readLn
         putStr "Entre com o comprimento: "
         comp <- readLn
         putStr "Entre com o modulo de elasticidade: "
         e <- readLn
         putStr "Entre com o momento de inércia: "
         i <- readLn
         let resultado = deflexaoViga c comp e i
         putStrLn $ "Resultado: " ++ (show resultado)
         hubMenus "civil"
         
      4 -> do
         putStr "Entre com o modulo de elasticidade: "
         e <- readLn
         putStr "Entre com o momento de inércia: "
         i <- readLn
         putStr "Entre com o comprimento: "
         comp <- readLn
         let resultado = cargaCriticaEuler e i comp
         putStrLn $ "Resultado: " ++ (show resultado)
         hubMenus "civil"
         
      5 -> do
         fig <- lerFigura
         let resultado = volumeConcreto fig
         putStrLn $ "Resultado: " ++ (show resultado)
         hubMenus "civil"
         
   where
      listaOp = [
        "[1] Momento de Inércia Retangular",
        "[2] Tensão Normal",
        "[3] Deflexão de Viga",
        "[4] Carga Crítica de Euler",
        "[5] Volume de Concreto",
        "[0] Voltar"]
      opMin = 0
      opMax = 5
      
menuEletrica :: IO ()
menuEletrica = do
   putStrLn "=== MENU ENGENHARIA ELÉTRICA ==="
   exibirOpções listaOp
   op <- lerOpção opMin opMax
   
   case op of
      0 -> do hubMenus "engenharias"
      1 -> do
         putStr "Entre com a corrente: "
         c <- readLn
         putStr "Entre com a resistência: "
         r <- readLn
         putStrLn $ "Resultado: " ++ (show (tensaoOhm c r))
         menuEletrica
      2 -> do
         putStr "Entre com a tensão: "
         t <- readLn
         putStr "Entre com a corrente: "
         c <- readLn
         putStrLn $ "Resultado: " ++ (show (potenciaEletricaVI t c))
         menuEletrica
      3 -> do
         putStr "Entre com a resistência: "
         r <- readLn
         putStr "Entre com a corrente: "
         c <- readLn
         putStrLn $ "Resultado: " ++ (show (potenciaEletricaRI r c))
         menuEletrica
      4 -> do
         putStr "Entre com a tensão: "
         t <- readLn
         putStr "Entre com a resistência: "
         r <- readLn
         putStrLn $ "Resultado: " ++ (show (potenciaEletricaVR t r))
         menuEletrica
      5 -> do
         putStr "Entre com a lista de resistências (ex: [1.2, 3.4, 5.6]): "
         l <- readLn
         putStrLn $ "Resultado: " ++ (show (resistenciaSerie l))
         menuEletrica
      6 -> do
         putStr "Entre com a lista de resistências (ex: [1.2, 3.4, 5.6]): "
         l <- readLn
         putStrLn $ "Resultado: " ++ (show (resistenciaParalelo l))
         menuEletrica
      7 -> do
         putStr "Entre com a resistência: "
         r <- readLn
         putStr "Entre com a reatância: "
         re <- readLn
         putStrLn $ "Resultado: " ++ (show (impedanciaAC r re))
         menuEletrica
      8 -> do
         putStr "Entre com a coordenada radial: "
         c <- readLn
         putStr "Entre com o ângulo em radianos: "
         a <- readLn
         putStrLn $ "Resultado: " ++ (show (polarParaRetangular c a))
         menuEletrica
      9 -> do
         putStr "Entre com a coordenada x: "
         x <- readLn
         putStr "Entre com a coordenada y: "
         y <- readLn
         putStrLn $ "Resultado: " ++ (show (retangularParaPolar x y))
         menuEletrica
   where
      listaOp = [
        "[1] Tensão (Lei de Ohm)",
        "[2] Potência Elétrica (V*I)",
        "[3] Potência Elétrica (R*I^2)",
        "[4] Potência Elétrica (V^2/R)",
        "[5] Resistência em Série",
        "[6] Resistência em Paralelo",
        "[7] Impedância AC",
        "[8] Polar para Retangular",
        "[9] Retangular para Polar",
        "[0] Voltar"]
      opMin = 0
      opMax = 9
      
menuMecanica :: IO ()
menuMecanica = do
   putStrLn "=== MENU ENGENHARIA MECÂNICA ==="
   exibirOpções listaOp
   op <- lerOpção opMin opMax
   
   case op of
      0 -> do hubMenus "engenharias"
      1 -> do
         putStr "Entre com a força: "
         f <- readLn
         putStr "Entre com a distância do eixo: "
         d <- readLn
         putStr "Entre com o ângulo em radianos: "
         a <- readLn
         putStrLn $ "Resultado: " ++ (show (calcularTorque f d a))
         menuMecanica
      2 -> do
         putStr "Entre com a velocidade linear: "
         v <- readLn
         putStr "Entre com o raio: "
         r <- readLn
         putStrLn $ "Resultado: " ++ (show (velocidadeAngular v r))
         menuMecanica
      3 -> do
         putStr "Entre com a velocidade tangencial: "
         v <- readLn
         putStr "Entre com o raio: "
         r <- readLn
         putStrLn $ "Resultado: " ++ (show (aceleracaocentripeta v r))
         menuMecanica
      4 -> do
         putStr "Entre com a massa: "
         m <- readLn
         putStr "Entre com a velocidade: "
         v <- readLn
         putStrLn $ "Resultado: " ++ (show (energiaCinetica m v))
         menuMecanica
      5 -> do
         putStr "Entre com a massa: "
         m <- readLn
         putStr "Entre com a altura: "
         a <- readLn
         putStrLn $ "Resultado: " ++ (show (energiaPotencial m a))
         menuMecanica
      6 -> do
         putStr "Entre com a lista de pares (massa, distância): "
         l <- readLn
         putStrLn $ "Resultado: " ++ (show (centroMassaX l))
         menuMecanica
         
   where
      listaOp = [
        "[1] Calcular Torque",
        "[2] Velocidade Angular",
        "[3] Aceleração Centrípeta",
        "[4] Energia Cinética",
        "[5] Energia Potencial",
        "[6] Centro de Massa X",
        "[0] Voltar"]
      opMin = 0
      opMax = 6
