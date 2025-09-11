module Calculus where

import Types

avaliarFuncao :: Funcao -> Double -> Double                                   
avaliarFuncao ( Funcao ( Linear a b ) _ ) x = a * x + b                            -- Representação de uma função do tipo ax + b;
avaliarFuncao ( Funcao ( Quadratica a b c ) _ ) x = a * ( x ** 2 ) + b * x + c     -- Representação de uma função do tipo ax^2 + bx + c ;
avaliarFuncao ( Funcao ( Exponencial a b ) _ ) x = a * exp ( b * x )               -- Representação de uma função do tipo a * e ^ ( bx);
avaliarFuncao ( Funcao ( Logaritmica a b ) _ ) x                                   -- Representação de uma função do tipo a * log ( bx );
        | x <= 0     = error "Erro: Logaritmo indefinido "
        | b * x <= 0 = error "Erro: Argumento inválido"
        | otherwise  = a * log ( b * x )
avaliarFuncao ( Funcao ( Trigonometrica tipo a b ) _ ) x =                         
    case tipo of
        Seno     -> a * sin ( x ) + b                                              -- Representação de uma função do tipo a * sen x + b;
        Cosseno  -> a * cos ( x ) + b                                              -- Representação de uma função do tipo a * cos x + b;
        Tangente -> a * tan ( x ) + b                                              -- Representação de uma função do tipo a * tan x + b;

integralNumerica :: Funcao -> Double -> Double -> Int -> Double  -- Cálculo da Integral, por meio da soma de Reimann;             
integralNumerica funcao limite_inferior limite_superior n                      
   | limite_inferior > limite_superior = error "Erro: limite inferior maior do que limite superior, não é possível calcular a integral"  -- Erro caso os limites de integração sejam do tipo [ a, b ] tal que b > a;
   | otherwise = sum [ avaliarFuncao funcao ( limite_inferior + base * fromIntegral posicao ) * base                                     -- Realiza a soma de Reimann, por meio da soma de f(x) * base n vezes;
            | posicao <- [0 .. n - 1]]                                         
     where base = ( limite_superior - limite_inferior ) / fromIntegral n                                                                 -- Calculo da base;      

derivadaNumerica :: Funcao-> Double-> Double  -- Definição newtoniana de derivadas -> f'(x) = (f ( x + h ) - f (x))/h , com h sendo um número pequeno;                               
derivadaNumerica funcao x = 
    ( avaliarFuncao funcao ( x + dx ) - avaliarFuncao funcao ( x )) / dx          
        where dx = 1e-9

encontrarRaizes :: Funcao -> Double -> Double -> [Double]  -- Percorre um intervalo numérico do tipo [a,b] e calcula o f(x), caso for 0 ou muito proximo de 0 é adicionado a lista;
encontrarRaizes funcao limite_inferior limite_superior =
    [ x | x <- [limite_inferior, limite_inferior + dx .. limite_superior - dx] 
    , let fx = avaliarFuncao funcao x
    , abs fx < tol ]
          where
            dx = 1e-3
            tol = 1e-6

pontosCriticos :: Funcao -> Double -> Double -> [Double] -- Percorre um intervalo numérico do tipo [a,b] e calcula a derivada de x , caso for 0 ou muito proximo de 0 é adicionado a lista;
pontosCriticos funcao limite_inferior limite_superior =
    [ x
    | x <- [limite_inferior, limite_inferior + dx .. limite_superior - dx]    
    , let derivada = derivadaNumerica funcao x
    , abs derivada < tol
    ]
  where
    dx  = 1e-5   
    tol = 1e-4   

ponto_maximo :: Funcao -> [Double] -> Maybe Double -- Encontra o ponto de uma lista, que seja aquele que possui maior f(x)
ponto_maximo _ []     = Nothing
ponto_maximo _ [x]    = Just x
ponto_maximo funcao (a:b:xs)
    | avaliarFuncao funcao  a >= avaliarFuncao funcao  b = ponto_maximo funcao  (a:xs)
    | otherwise                                          = ponto_maximo funcao  (b:xs)

ponto_minimo :: Funcao -> [Double] -> Maybe Double -- Encontra o ponto de uma lista, que seja aquele que possui o menor f(x)
ponto_minimo _ []     = Nothing
ponto_minimo _ [x]    = Just x
ponto_minimo funcao (a:b:xs)
    | avaliarFuncao funcao  a <= avaliarFuncao funcao  b = ponto_minimo funcao  (a:xs)
    | otherwise                                          = ponto_minimo funcao  (b:xs)

encontrarMaximo :: Funcao -> Double -> Double -> Maybe Double -- Encontra os pontos máximos de uma função, aparir de seu críticos
encontrarMaximo funcao limite_inferior limite_superior
    | limite_inferior > limite_superior = Nothing
    | otherwise = ponto_maximo funcao pontos
  where 
    criticos = pontosCriticos funcao limite_inferior limite_superior
    pontos   = limite_inferior : limite_superior : criticos

encontrarMinimo :: Funcao -> Double -> Double -> Maybe Double -- Encontra os pontos mínimos de uma função, aparir de seu críticos
encontrarMinimo funcao limite_inferior limite_superior
    | limite_inferior > limite_superior = Nothing
    | otherwise = ponto_minimo funcao pontos
  where 
    criticos = pontosCriticos funcao limite_inferior limite_superior
    pontos   = limite_inferior : limite_superior : criticos
            
        
