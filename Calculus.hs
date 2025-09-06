module Calculus where

import Types

integralNumerica :: Funcao -> Double -> Double -> Int -> Double                -- Cálculo de Integral com Base na Soma de Riemann;
integralNumerica funcao limite_inferior limite_superior n =                    -- limites de integração e a quantidade de retângulos para representar a função;
   | limite_inferior > limite_superior = error "Erro: limite inferior maior do que limite superior, não é possível calcular a integral" 
   | otherwise = sum [ avaliarFuncao funcao ( limite_inferior + base * fromIntegral posicao ) * base 
            | posicao <- [0 .. n - 1]]                                         -- Realiza a Soma das àrea dos retângulos, área calculada por f (x) * base;
     where base = ( limite_superior - limite_inferior ) / fromIntegral n       -- base de cada retângulo;

derivadaNumerica :: Funcao-> Double-> Double                                   -- Cálculo de Derivada pela definição newtoniana
derivadaNumerica funcao x = 
    ( avaliarFuncao funcao ( x + dx ) - avaliarFuncao funcao ( x )) / dx       -- f' (x) ~= (f ( x + dx ) + f (x)) / dx
        where dx = 1e-9

pontosCriticos :: Funcao -> Double -> Double -> [Double]                       -- Cálculo dos pontos candidatos à serem críticos;
pontosCriticos funcao limite_inferior limite_superior =
    [ x | x <- [limite_inferior, limite_inferior + dx .. limite_superior - dx] -- Passa por pontos do intervalo;
    , let derivada1 = derivadaNumerica funcao x                                -- Cálculo da derivada numericas do ponto e de seu sucessor;
          derivada2 = derivadaNumerica funcao (x + dx)
    , derivada1 * derivada2 <= 0 ]                                             -- Por definição se a derivada númerica trocar de sinal, o ponto é um candidato à máximo ou mínimo ;
     where
        dx = 1e-4
        
encontrarMaximo :: Funcao -> Double -> Double -> Maybe Double                  -- Calcúlo do máximo de um intervalo numérico;
encontrarMaximo funcao limite_inferior limite_superior
    | limite_inferior > limite_superior = Nothing                              -- Intervalo inválido, não é possível calcular os pontos de máximo;
    | null pontos                       = Just ( maximum [avaliarFuncao funcao limite_inferior , avaliarFuncao funcao limite_superior ]) -- Caso não houver pontos candidatos à critico;
    | otherwise                         = Just ( maximum fx )                  -- Encontra o maior valor que a função pode atingir em dado intervalo
         where 
            candidatos = pontosCriticos funcao limite_inferior limite_superior
            pontos = limite_inferior : limite_superior : candidatos
            fx = map (avaliarFuncao funcao) pontos

encontrarMinimo :: Funcao -> Double -> Double -> Maybe Double                  -- Calcúlo do mínimo de um intervalo numérico;
encontrarMinimo funcao limite_inferior limite_superior
    | limite_inferior > limite_superior = Nothing                              -- Intervalo inválido, não é possível calcular os pontos de mínimo;
    | null pontos                       = Just ( minimum [avaliarFuncao funcao limite_inferior , avaliarFuncao funcao limite_superior ]) -- Caso não houver pontos candidatos à critico;
    | otherwise                         = Just ( minimum fx )                  -- Encontra o menor valor que a função pode atingir em dado intervalo;
        where 
            candidatos = pontosCriticos funcao limite_inferior limite_superior
            pontos = limite_inferior : limite_superior : candidatos
            fx = map (avaliarFuncao funcao) pontos
            
calcularComprimentoCurva :: Funcao -> Double -> Double -> Comprimento          -- Uso da definição de cálculo de comprimento de curvas utilizando integral e derivadas
calcularComprimentoCurva funcao limite_inferior limite_superior = 
    integralNumerica f limite_inferior limite_superior 10000
       where f x = sqrt ( 1 + ( derivadaNumerica funcao x ) ^ 2 ) 
