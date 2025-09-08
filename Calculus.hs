module Calculus where

import Types

avaliarFuncao :: Funcao -> Double -> Double                                   -- Definição dos tipos das funções, com o cálculo de f (x) para todas os tipos de funções
avaliarFuncao ( Funcao ( Linear a b ) _ ) x = a * x + b
avaliarFuncao ( Funcao ( Quadratica a b c ) _ ) x = a * ( x ** 2 ) + b * x + c
avaliarFuncao ( Funcao ( Exponencial a b ) _ ) x = a * exp ( b * x )
avaliarFuncao ( Funcao ( Logaritmica a b ) _ ) x
        | x <= 0     = error "Erro: Logaritmo indefinido "
        | b * x <= 0 = error "Erro: Argumento inválido"
        | otherwise  = a * log ( b * x )
avaliarFuncao ( Funcao ( Trigonometrica tipo a b ) _ ) x =
    case tipo of
        Seno     -> a * sin ( x ) + b 
        Cosseno  -> a * cos ( x ) + b
        Tangente -> a * tan ( x ) + b

integralNumerica :: Funcao -> Double -> Double -> Int -> Double                -- Cálculo de Integral com Base na Soma de Riemann;
integralNumerica funcao limite_inferior limite_superior n                      -- limites de integração e a quantidade de retângulos para representar a função;
   | limite_inferior > limite_superior = error "Erro: limite inferior maior do que limite superior, não é possível calcular a integral" 
   | otherwise = sum [ avaliarFuncao funcao ( limite_inferior + base * fromIntegral posicao ) * base 
            | posicao <- [0 .. n - 1]]                                         -- Realiza a Soma das àrea dos retângulos, área calculada por f (x) * base;
     where base = ( limite_superior - limite_inferior ) / fromIntegral n       -- base de cada retângulo;

derivadaNumerica :: Funcao-> Double-> Double                                   -- Cálculo de Derivada pela definição newtoniana
derivadaNumerica funcao x = 
    ( avaliarFuncao funcao ( x + dx ) - avaliarFuncao funcao ( x )) / dx       -- f' (x) ~= (f ( x + dx ) - f (x)) / dx
        where dx = 1e-9

encontrarRaizes :: Funcao -> Double -> Double -> [Double]
encontrarRaizes funcao limite_inferior limite_superior = 
    [ x | x <- [limite_inferior, limite_inferior + dx .. limite_superior - dx]
    , let fx = avaliarFuncao funcao x
    , abs fx < tol ]
          where
            dx = 1e-3
            tol = 1e-6

pontosCriticos :: Funcao -> Double -> Double -> [Double]
pontosCriticos funcao limite_inferior limite_superior =
    [ x
    | x <- [limite_inferior, limite_inferior + dx .. limite_superior - dx]
    , let derivada = derivadaNumerica funcao x
    , abs derivada < tol
    ]
  where
    dx  = 1e-5   
    tol = 1e-4   

ponto_maximo :: Funcao -> [Double] -> Maybe Double
ponto_maximo _ []     = Nothing
ponto_maximo _ [x]    = Just x
ponto_maximo funcao (a:b:xs)
    | avaliarFuncao funcao  a >= avaliarFuncao funcao  b = ponto_maximo funcao  (a:xs)
    | otherwise                                          = ponto_maximo funcao  (b:xs)

ponto_minimo :: Funcao -> [Double] -> Maybe Double
ponto_minimo _ []     = Nothing
ponto_minimo _ [x]    = Just x
ponto_minimo funcao (a:b:xs)
    | avaliarFuncao funcao  a <= avaliarFuncao funcao  b = ponto_minimo funcao  (a:xs)
    | otherwise                                          = ponto_minimo funcao  (b:xs)

encontrarMaximo :: Funcao -> Double -> Double -> Maybe Double
encontrarMaximo funcao limite_inferior limite_superior
    | limite_inferior > limite_superior = Nothing
    | otherwise = ponto_maximo funcao pontos
  where 
    criticos = pontosCriticos funcao limite_inferior limite_superior
    pontos   = limite_inferior : limite_superior : criticos

encontrarMinimo :: Funcao -> Double -> Double -> Maybe Double
encontrarMinimo funcao limite_inferior limite_superior
    | limite_inferior > limite_superior = Nothing
    | otherwise = ponto_minimo funcao pontos
  where 
    criticos = pontosCriticos funcao limite_inferior limite_superior
    pontos   = limite_inferior : limite_superior : criticos
            
calcularComprimentoCurva :: Funcao -> Double -> Double -> Comprimento          
calcularComprimentoCurva funcao limite_inferior limite_superior = 
    integralNumerica f  limite_inferior limite_superior 10000
       where f x = sqrt ( 1 + ( derivadaNumerica funcao x ) ^ 2 ) 
