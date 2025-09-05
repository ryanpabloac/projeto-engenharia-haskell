module Calculus where

import Types

integralNumerica :: Funcao -> Double -> Double -> Int -> Double -- Cálculo de Integral com Base na Soma de Riemann;
integralNumerica funcao limite_inferior limite_superior n = -- limites de integração e a quantidade de retângulos para representar a função;
    sum [ avaliarFuncao funcao ( limite_inferior + base * fromIntegral posicao ) * base | posicao <- [0 .. n - 1]] -- Realiza a Soma das àrea dos retângulos, área calculada por f (x) * base;
        where 
            base = ( limite_superior - limite_inferior ) / fromIntegral n -- base de cada retângulo;

derivadaNumerica :: Funcao-> Double-> Double -- Cálculo de Derivada pela definição newtoniana
derivadaNumerica funcao x = 
        ( avaliarFuncao funcao ( x + dx ) - avaliarFuncao funcao ( x )) / dx -- f' (x) ~= (f ( x + dx ) + f (x)) / dx
        where 
            dx = 1e-9
