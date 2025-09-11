module Tests.CalculusTests where

import Types
import Calculus

-- Testes para avaliarFuncao
testeAvaliacaoLinear :: Bool
testeAvaliacaoLinear =
    let funcao = Funcao (Linear 2 3) "f(x) = 2x + 3" -- f(x) = 2x + 3
        resultado = avaliarFuncao funcao 5
    in abs (resultado - 13.0) < 0.001 -- 2*5 + 3 = 13

testeAvaliacaoLinearZero :: Bool
testeAvaliacaoLinearZero =
    let funcao = Funcao (Linear 2 3) "f(x) = 2x + 3"
        resultado = avaliarFuncao funcao 0
    in abs (resultado - 3.0) < 0.001 -- 2*0 + 3 = 3

testeAvaliacaoLinearNegativo :: Bool
testeAvaliacaoLinearNegativo =
    let funcao = Funcao (Linear 2 3) "f(x) = 2x + 3"
        resultado = avaliarFuncao funcao (-1.5)
    in abs (resultado - 0.0) < 0.001 -- 2*(-1.5) + 3 = 0

testeAvaliacaoQuadratica :: Bool
testeAvaliacaoQuadratica =
    let funcao = Funcao (Quadratica 1 0 0) "f(x) = x²" -- f(x) = x²
        resultado = avaliarFuncao funcao 4
    in abs (resultado - 16.0) < 0.001

testeAvaliacaoQuadraticaCompleta :: Bool
testeAvaliacaoQuadraticaCompleta =
    let funcao = Funcao (Quadratica 2 (-3) 1) "f(x) = 2x² - 3x + 1"
        resultado = avaliarFuncao funcao 2
        esperado = 2*4 - 3*2 + 1 -- 8 - 6 + 1 = 3
    in abs (resultado - esperado) < 0.001

testeAvaliacaoExponencial :: Bool
testeAvaliacaoExponencial =
    let funcao = Funcao (Exponencial 1 1) "f(x) = e^x" -- f(x) = e^x
        resultado = avaliarFuncao funcao 0
    in abs (resultado - 1.0) < 0.001 -- e^0 = 1

testeAvaliacaoExponencialEscalada :: Bool
testeAvaliacaoExponencialEscalada =
    let funcao = Funcao (Exponencial 2 0.5) "f(x) = 2e^(0.5x)"
        resultado = avaliarFuncao funcao 0
    in abs (resultado - 2.0) < 0.001 -- 2*e^0 = 2

testeAvaliacaoLogaritmica :: Bool
testeAvaliacaoLogaritmica =
    let funcao = Funcao (Logaritmica 1 1) "f(x) = ln(x)" -- f(x) = ln(x)
        resultado = avaliarFuncao funcao (exp 1)
    in abs (resultado - 1.0) < 0.001 -- ln(e) = 1

testeAvaliacaoLogaritmicaEscalada :: Bool
testeAvaliacaoLogaritmicaEscalada =
    let funcao = Funcao (Logaritmica 2 1) "f(x) = 2ln(x)"
        resultado = avaliarFuncao funcao 1
    in abs (resultado - 0.0) < 0.001 -- 2*ln(1) = 0

testeAvaliacaoTrigonometrica :: Bool
testeAvaliacaoTrigonometrica =
    let funcao = Funcao (Trigonometrica Seno 1 0) "f(x) = sin(x)"
        resultado = avaliarFuncao funcao (pi/2)
    in abs (resultado - 1.0) < 0.001 -- sin(§π§/2) = 1

-- Testes para derivadaNumerica
testeDerivadaLinear :: Bool
testeDerivadaLinear =
    let funcao = Funcao (Linear 5 2) "f(x) = 5x + 2" -- f'(x) = 5
        derivada = derivadaNumerica funcao 10 -- derivada em qualquer ponto
    in abs (derivada - 5.0) < 0.1

testeDerivadaConstante :: Bool
testeDerivadaConstante =
    let funcao = Funcao (Linear 0 7) "f(x) = 7" -- f'(x) = 0
        derivada = derivadaNumerica funcao 5
    in abs derivada < 0.1

testeDerivadaQuadratica :: Bool
testeDerivadaQuadratica =
    let funcao = Funcao (Quadratica 1 0 0) "f(x) = x²" -- f'(x) = 2x
        derivadaEm3 = derivadaNumerica funcao 3 -- f'(3) = 6
    in abs (derivadaEm3 - 6.0) < 0.1

testeDerivadaQuadraticaVertice :: Bool
testeDerivadaQuadraticaVertice =
    let funcao = Funcao (Quadratica 1 0 0) "f(x) = x²"
        derivadaEm0 = derivadaNumerica funcao 0 -- f'(0) = 0
    in abs derivadaEm0 < 0.1

testeDerivadaSeno :: Bool
testeDerivadaSeno =
    let funcao = Funcao (Trigonometrica Seno 1 0) "f(x) = sin(x)"
        derivadaEm0 = derivadaNumerica funcao 0 -- f'(0) = cos(0) = 1
    in abs (derivadaEm0 - 1.0) < 0.1

-- Testes para integralNumerica
testeIntegralLinear :: Bool
testeIntegralLinear =
    let funcao = Funcao (Linear 2 0) "f(x) = 2x"
        integral = integralNumerica funcao 0 3 1000
    in abs (integral - 9.0) < 0.01

testeIntegralConstante :: Bool
testeIntegralConstante =
    let funcao = Funcao (Linear 0 5) "f(x) = 5"
        integral = integralNumerica funcao 0 2 1000
    in abs (integral - 10.0) < 0.01

testeIntegralQuadratica :: Bool
testeIntegralQuadratica =
    let funcao = Funcao (Quadratica 1 0 0) "f(x) = x²"
        integral = integralNumerica funcao 0 2 1000
    in abs (integral - (8/3)) < 0.01

testeIntegralZero :: Bool
testeIntegralZero =
    let funcao = Funcao (Linear 1 0) "f(x) = x"
        integral = integralNumerica funcao 0 0 1000
    in abs integral < 0.001

testeIntegralSimetrica :: Bool
testeIntegralSimetrica =
    let funcao = Funcao (Trigonometrica Seno 1 0) "f(x) = sin(x)"
        integral = integralNumerica funcao (-pi) pi 1000
    in abs integral < 0.1

-- Testes para encontrarRaizes
testeRaizLinear :: Bool
testeRaizLinear =
    let funcao = Funcao (Linear 1 (-3)) "f(x) = x - 3" -- raiz em x = 3
        raizes = encontrarRaizes funcao 0 5
    in length raizes == 1 && abs (head raizes - 3.0) < 0.1

testeRaizQuadratica :: Bool
testeRaizQuadratica =
    let funcao = Funcao (Quadratica 1 0 (-4)) "f(x) = x² - 4" -- ra´ızes em ±2
        raizes = encontrarRaizes funcao (-3) 3
    in length raizes == 2

testeSemRaizes :: Bool
testeSemRaizes =
    let funcao = Funcao (Quadratica 1 0 1) "f(x) = x² + 1" -- sem ra´ızes reais
        raizes = encontrarRaizes funcao (-5) 5
    in null raizes

testeRaizDupla :: Bool
testeRaizDupla =
    let funcao = Funcao (Quadratica 1 (-4) 4) "f(x) = (x-2)²" -- raiz dupla em x=2
        raizes = encontrarRaizes funcao 0 4
    in length raizes >= 1 && any (\x -> abs (x - 2.0) < 0.1) raizes

testeRaizConstante :: Bool
testeRaizConstante =
    let funcao = Funcao (Linear 0 5) "f(x) = 5" -- fun¸c~ao constante, sem ra´ızes
        raizes = encontrarRaizes funcao (-10) 10
    in null raizes

-- Testes para encontrarMaximo
testeMaximoParabola :: Bool
testeMaximoParabola =
    let funcao = Funcao (Quadratica (-1) 0 4) "f(x) = -x² + 4" -- m´aximo em x=0
        maximo = encontrarMaximo funcao (-2) 2
    in case maximo of
        Just x -> abs x < 0.1 -- m´aximo pr´oximo de x=0
        Nothing -> False

testeMaximoLinearCrescente :: Bool
testeMaximoLinearCrescente =
    let funcao = Funcao (Linear 1 0) "f(x) = x" -- fun¸c~ao crescente
        maximo = encontrarMaximo funcao 0 5
    in case maximo of
        Just x -> abs (x - 5.0) < 0.1 -- m´aximo no limite superior
        Nothing -> False

testeMaximoLinearDecrescente :: Bool
testeMaximoLinearDecrescente =
    let funcao = Funcao (Linear (-1) 0) "f(x) = -x" -- fun¸c~ao decrescente
        maximo = encontrarMaximo funcao 0 5
    in case maximo of
        Just x -> abs x < 0.1 -- m´aximo no limite inferior
        Nothing -> False

testeMaximoConstante :: Bool
testeMaximoConstante =
    let funcao = Funcao (Linear 0 5) "f(x) = 5" -- fun¸c~ao constante
        maximo = encontrarMaximo funcao 0 5
    in case maximo of
        Just x -> x >= 0 && x <= 5 -- qualquer ponto ´e m´aximo
        Nothing -> False

testeMaximoSeno :: Bool
testeMaximoSeno =
    let funcao = Funcao (Trigonometrica Seno 1 0) "f(x) = sin(x)"
        maximo = encontrarMaximo funcao 0 pi
    in case maximo of
        Just x -> abs (x - pi/2) < 0.1 -- m´aximo em §π/2§
        Nothing -> False

-- Testes para encontrarMinimo
testeMinimoParabola :: Bool
testeMinimoParabola =
    let funcao = Funcao (Quadratica 1 0 0) "f(x) = x²" -- m´ınimo em x=0
        minimo = encontrarMinimo funcao (-2) 2
    in case minimo of
        Just x -> abs x < 0.1 -- m´ınimo pr´oximo de x=0
        Nothing -> False

testeMinimoLinearCrescente :: Bool
testeMinimoLinearCrescente =
    let funcao = Funcao (Linear 1 0) "f(x) = x"
        minimo = encontrarMinimo funcao 0 5
    in case minimo of
        Just x -> abs x < 0.1 -- m´ınimo no limite inferior
        Nothing -> False

testeMinimoParabolaDeslocada :: Bool
testeMinimoParabolaDeslocada =
    let funcao = Funcao (Quadratica 1 (-4) 4) "f(x) = (x-2)²" -- m´ınimo em x=2
        minimo = encontrarMinimo funcao 0 4
    in case minimo of
        Just x -> abs (x - 2.0) < 0.1
        Nothing -> False

testeMinimoFuncaoInvertida :: Bool
testeMinimoFuncaoInvertida =
    let funcao = Funcao (Quadratica (-1) 0 0) "f(x) = -x²" -- m´aximo em x=0, n~ao m´ınimo 
        minimo = encontrarMinimo funcao (-2) 2
    in case minimo of
        Just x -> abs x > 1.5 -- m´ınimo nos extremos
        Nothing -> False

testeMinimoConstante :: Bool
testeMinimoConstante =
    let funcao = Funcao (Linear 0 3) "f(x) = 3"
        minimo = encontrarMinimo funcao (-1) 1
    in case minimo of
        Just x -> x >= -1 && x <= 1 -- qualquer ponto ´e m´ınimo
        Nothing -> False
{-
-- Testes para calcularComprimentoCurva
testeComprimentoRetaHorizontal :: Bool
testeComprimentoRetaHorizontal =
    let funcao = Funcao (Linear 0 5) "f(x) = 5" -- reta horizontal
        comprimento = calcularComprimentoCurva funcao 0 10
    in abs (comprimento - 10.0) < 0.1 -- comprimento = diferen¸ca em x

testeComprimentoRetaInclinada :: Bool
testeComprimentoRetaInclinada =
    let funcao = Funcao (Linear 1 0) "f(x) = x" -- reta y=x
        comprimento = calcularComprimentoCurva funcao 0 1
        esperado = sqrt 2 -- hipotenusa do tri^angulo (1,1)
    in abs (comprimento - esperado) < 0.1

testeComprimentoRetaVertical :: Bool
testeComprimentoRetaVertical =
    let funcao = Funcao (Linear 0 0) "f(x) = 0" -- eixo x
        comprimento = calcularComprimentoCurva funcao 0 5
    in abs (comprimento - 5.0) < 0.1

testeComprimentoParabola :: Bool
testeComprimentoParabola =
    let funcao = Funcao (Quadratica 1 0 0) "f(x) = x²"
        comprimento = calcularComprimentoCurva funcao 0 1
    in comprimento > 1.0 && comprimento < 2.0 -- deve ser maior que a corda

testeComprimentoConstante :: Bool
testeComprimentoConstante =
    let funcao = Funcao (Linear 0 (-3)) "f(x) = -3" -- reta horizontal negativa
        comprimento = calcularComprimentoCurva funcao (-2) 3
    in abs (comprimento - 5.0) < 0.1 -- diferen¸ca entre -2 e 3
-}
