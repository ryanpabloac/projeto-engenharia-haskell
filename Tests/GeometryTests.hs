module  Tests.GeometryTests  where

import Types
import Geometry

-- Testes para distanciaEntrePontos 
testeDistancia2DBasico :: Bool 
testeDistancia2DBasico =
    let p1 = Ponto2D 0 0
        p2 = Ponto2D 3 4
        resultado = distanciaEntrePontos p1 p2 
    in abs (resultado - 5.0) < 0.001

testeDistancia2DMesmosPontos :: Bool 
testeDistancia2DMesmosPontos =
    let p1 = Ponto2D 5 7
        resultado = distanciaEntrePontos p1 p1 
    in abs resultado < 0.001

testeDistancia2DNegativa :: Bool 
testeDistancia2DNegativa =
    let p1 = Ponto2D (-2) (-3)
        p2 = Ponto2D 1 1
        resultado  =  distanciaEntrePontos  p1  p2
        esperado = sqrt ((1-(-2))^2 + (1-(-3))^2)       -- sqrt(9+16) = 5
    in abs (resultado - esperado) < 0.001

testeDistancia2DDecimal :: Bool 
testeDistancia2DDecimal =
    let p1 = Ponto2D 1.5 2.5
        p2 = Ponto2D 4.5 6.5
        resultado  =  distanciaEntrePontos  p1  p2
        esperado = sqrt ((4.5-1.5)^2 + (6.5-2.5)^2)     -- sqrt(9+16) = 5
    in abs (resultado - esperado) < 0.001

testeDistancia2DGrande :: Bool 
testeDistancia2DGrande =
    let p1 = Ponto2D 0 0
        p2 = Ponto2D 1000000 1000000
        resultado = distanciaEntrePontos p1 p2 
        esperado = sqrt (2 * 1000000^2)
    in abs (resultado - esperado) < 1000               -- tolerância maior para números grandes

-- Testes para distancia3D
testeDistancia3DBasico :: Bool 
testeDistancia3DBasico =
    let p1 = Ponto3D 1 2 2
        p2 = Ponto3D 4 6 2
        resultado = distancia3D p1 p2
        esperado = 5.0                                 -- sqrt((4-1)² + (6-2)² + (2-2)²) = sqrt(9+16+0)
    in abs (resultado - esperado) < 0.001

testeDistancia3DMesmosPontos :: Bool 
testeDistancia3DMesmosPontos =
    let p1 = Ponto3D 3 4 5
        resultado = distancia3D p1 p1 
    in abs resultado < 0.001

testeDistancia3DCompleto :: Bool 
testeDistancia3DCompleto =
    let p1 = Ponto3D 0 0 0
        p2 = Ponto3D 2 3 6
        resultado = distancia3D p1 p2
        esperado = sqrt (2^2 + 3^2 + 6^2)            -- sqrt(4+9+36) = 7
    in abs (resultado - esperado) < 0.001

testeDistancia3DNegativa :: Bool 
testeDistancia3DNegativa =
    let p1 = Ponto3D (-1) (-2) (-3)
        p2 = Ponto3D 1 2 3
        resultado = distancia3D p1 p2
        esperado = sqrt (2^2 + 4^2 + 6^2)            -- sqrt(4+16+36) = sqrt(56)
    in abs (resultado - esperado) < 0.001

testeDistancia3DPequena :: Bool 
testeDistancia3DPequena =
    let p1 = Ponto3D 0.001 0.001 0.001
        p2 = Ponto3D 0.002 0.002 0.002
        resultado = distancia3D p1 p2 
        esperado = sqrt (3 * 0.001^2)
    in abs (resultado - esperado) < 0.0001

-- Testes para pontoMedio
testePontoMedioBasico :: Bool 
testePontoMedioBasico =
     let p1 = Ponto2D 0 0
         p2 = Ponto2D 4 6
         resultado = pontoMedio p1 p2 
         esperado = Ponto2D 2 3
     in resultado == esperado

testePontoMedioNegativo :: Bool 
testePontoMedioNegativo =
    let p1 = Ponto2D (-2) (-4)
        p2 = Ponto2D 2 4
        resultado = pontoMedio p1 p2 
        esperado = Ponto2D 0 0
    in resultado == esperado 

testePontoMedioMesmoPonto :: Bool
testePontoMedioMesmoPonto = 
    let p1 = Ponto2D 5 7
        resultado = pontoMedio p1 p1 
    in resultado == p1

testePontoMedioDecimal :: Bool 
testePontoMedioDecimal =
    let p1 = Ponto2D 1.5 2.5
        p2 = Ponto2D 3.5 4.5 
        resultado = pontoMedio p1 p2 
        esperado = Ponto2D 2.5 3.5
    in resultado == esperado

testePontoMedioDistante :: Bool 
testePontoMedioDistante =
    let p1 = Ponto2D (-1000) (-2000)
        p2 = Ponto2D 1000 2000
        resultado = pontoMedio p1 p2 
        esperado = Ponto2D 0 0
    in resultado == esperado

-- Testes para calcularArea
testeAreaRetangulo :: Bool 
testeAreaRetangulo =
    let retangulo = Retangulo 5 3 
        area = calcularArea retangulo
    in abs (area - 15.0) < 0.001

testeAreaCirculo :: Bool 
testeAreaCirculo =
    let circulo = Circulo 2
        area = calcularArea circulo 
        esperado = pi * 4           -- §π * r²§
    in abs (area - esperado) < 0.001

testeAreaTriangulo :: Bool
testeAreaTriangulo =
    let p1 = Ponto2D 0 0
        p2 = Ponto2D 4 0
        p3 = Ponto2D 0 3
        triangulo = Triangulo p1 p2 p3
        area = calcularArea triangulo
    in abs (area - 6.0) < 0.001 -- (base * altura) / 2
    
testeAreaRetanguloQuadrado :: Bool
testeAreaRetanguloQuadrado =
    let quadrado = Retangulo 4 4
        area = calcularArea quadrado
    in abs (area - 16.0) < 0.001

-- Testes para calcularVolume
testeVolumeEsfera :: Bool
testeVolumeEsfera =
    let esfera = Esfera 3
        volume = calcularVolume esfera
        esperado = (4/3) * pi * 27 -- (4/3) * §π§ * r³
    in abs (volume - esperado) < 0.001

testeVolumeCilindro :: Bool
testeVolumeCilindro =
    let cilindro = Cilindro 2 5
        volume = calcularVolume cilindro
        esperado = pi * 4 * 5 -- §π§ * r² * h
    in abs (volume - esperado) < 0.001

testeVolumeParalelepipedo :: Bool
testeVolumeParalelepipedo =
    let paralele = Paralelepipedo 2 3 4
        volume = calcularVolume paralele
    in abs (volume - 24.0) < 0.001 -- comprimento * largura * altura

testeVolumeEsferaUnitaria :: Bool
testeVolumeEsferaUnitaria =
    let esfera = Esfera 1
        volume = calcularVolume esfera
        esperado = (4/3) * pi
    in abs (volume - esperado) < 0.001

testeVolumeCilindroUnitario :: Bool
testeVolumeCilindroUnitario =
    let cilindro = Cilindro 1 1
        volume = calcularVolume cilindro
        esperado = pi
    in abs (volume - esperado) < 0.001
    
-- Testes para calcularPerimetro
testePerimetroRetangulo :: Bool
testePerimetroRetangulo =
    let retangulo = Retangulo 3 4
        perimetro = calcularPerimetro retangulo
    in abs (perimetro - 14.0) < 0.001 -- 2*(3+4)

testePerimetroCirculo :: Bool
testePerimetroCirculo =
    let circulo = Circulo 3
        perimetro = calcularPerimetro circulo
        esperado = 2 * pi * 3
    in abs (perimetro - esperado) < 0.001

testePerimetroCirculoUnitario :: Bool
testePerimetroCirculoUnitario =
    let circulo = Circulo 1
        perimetro = calcularPerimetro circulo
        esperado = 2 * pi
    in abs (perimetro - esperado) < 0.001

testePerimetroQuadrado :: Bool
testePerimetroQuadrado =
    let quadrado = Retangulo 5 5
        perimetro = calcularPerimetro quadrado
    in abs (perimetro - 20.0) < 0.001 -- 4*5

testePerimetroTriangulo :: Bool
testePerimetroTriangulo =
    let p1 = Ponto2D 0 0
        p2 = Ponto2D 3 0
        p3 = Ponto2D 0 4
        triangulo = Triangulo p1 p2 p3
        perimetro = calcularPerimetro triangulo
        esperado = 3 + 4 + 5 -- tri^angulo 3-4-5
    in abs (perimetro - esperado) < 0.001

-- Testes para dentroDoPoligono
testeDentroPoligonoQuadrado :: Bool
testeDentroPoligonoQuadrado =
    let quadrado = [Ponto2D 0 0, Ponto2D 2 0, Ponto2D 2 2, Ponto2D 0 2]
        pontoDentro = Ponto2D 1 1
        pontoFora = Ponto2D 3 3
    in dentroDoPoligono pontoDentro quadrado && not (dentroDoPoligono pontoFora quadrado)

testeDentroPoligonoTriangulo :: Bool
testeDentroPoligonoTriangulo =
    let triangulo = [Ponto2D 0 0, Ponto2D 4 0, Ponto2D 2 3]
        pontoDentro = Ponto2D 2 1
        pontoFora = Ponto2D 0 4
    in dentroDoPoligono pontoDentro triangulo && not (dentroDoPoligono pontoFora triangulo)

testeDentroPoligonoBorda :: Bool
testeDentroPoligonoBorda =
    let quadrado = [Ponto2D 0 0, Ponto2D 2 0, Ponto2D 2 2, Ponto2D 0 2]
        pontoBorda = Ponto2D 1 0 -- na borda inferior
    in dentroDoPoligono pontoBorda quadrado -- assumindo que borda conta como dentro

testeDentroPoligonoVertice :: Bool
testeDentroPoligonoVertice =
    let triangulo = [Ponto2D 0 0, Ponto2D 3 0, Ponto2D 1.5 2]
        pontoVertice = Ponto2D 0 0 -- no v´ertice
    in dentroDoPoligono pontoVertice triangulo

testeDentroPoligonoVazio :: Bool
testeDentroPoligonoVazio =
    let poligonoVazio = []
        ponto = Ponto2D 1 1
    in not (dentroDoPoligono ponto poligonoVazio)
    
-- Testes para intersecaoRetas
testeIntersecaoRetasBasico :: Bool
testeIntersecaoRetasBasico =
    let reta1 = (Ponto2D 0 0, Ponto2D 2 2) -- y = x
        reta2 = (Ponto2D 0 2, Ponto2D 2 0) -- y = -x + 2
        resultado = intersecaoRetas reta1 reta2
        esperado = Just (Ponto2D 1 1)
    in resultado == esperado

testeIntersecaoRetasParalelas :: Bool
testeIntersecaoRetasParalelas =
    let reta1 = (Ponto2D 0 0, Ponto2D 2 2) -- y = x
        reta2 = (Ponto2D 0 1, Ponto2D 2 3) -- y = x + 1
        resultado = intersecaoRetas reta1 reta2
    in resultado == Nothing -- retas paralelas n~ao se intersectam

testeIntersecaoRetasCoincidentes :: Bool
testeIntersecaoRetasCoincidentes =
    let reta1 = (Ponto2D 0 0, Ponto2D 2 2)
        reta2 = (Ponto2D 1 1, Ponto2D 3 3) -- mesma reta
        resultado = intersecaoRetas reta1 reta2
    in resultado == Nothing -- retas coincidentes t^em infinitos pontos

testeIntersecaoRetasPerpendicualres :: Bool
testeIntersecaoRetasPerpendicualres =
    let reta1 = (Ponto2D 0 1, Ponto2D 2 1) -- reta horizontal
        reta2 = (Ponto2D 1 0, Ponto2D 1 2) -- reta vertical
        resultado = intersecaoRetas reta1 reta2
        esperado = Just (Ponto2D 1 1)
    in resultado == esperado

testeIntersecaoRetasNegativas :: Bool
testeIntersecaoRetasNegativas =
    let reta1 = (Ponto2D (-2) (-2), Ponto2D 0 0) -- y = x
        reta2 = (Ponto2D (-2) 2, Ponto2D 0 0) -- y = -x
        resultado = intersecaoRetas reta1 reta2
        esperado = Just (Ponto2D 0 0)
    in resultado == esperado
    
    
-- Lista de testes: (nome do teste, resultado Bool)
todosTestes :: [(String, Bool)]
todosTestes =
    [ ("testeDistancia2DBasico", testeDistancia2DBasico)
    , ("testeDistancia2DMesmosPontos", testeDistancia2DMesmosPontos)
    , ("testeDistancia2DNegativa", testeDistancia2DNegativa)
    , ("testeDistancia2DDecimal", testeDistancia2DDecimal)
    , ("testeDistancia2DGrande", testeDistancia2DGrande)
    , ("testeDistancia3DBasico", testeDistancia3DBasico)
    , ("testeDistancia3DMesmosPontos", testeDistancia3DMesmosPontos)
    , ("testeDistancia3DCompleto", testeDistancia3DCompleto)
    , ("testeDistancia3DNegativa", testeDistancia3DNegativa)
    , ("testeDistancia3DPequena", testeDistancia3DPequena)
    , ("testePontoMedioBasico", testePontoMedioBasico)
    , ("testePontoMedioNegativo", testePontoMedioNegativo)
    , ("testePontoMedioMesmoPonto", testePontoMedioMesmoPonto)
    , ("testePontoMedioDecimal", testePontoMedioDecimal)
    , ("testePontoMedioDistante", testePontoMedioDistante)
    , ("testeAreaRetangulo", testeAreaRetangulo)
    , ("testeAreaCirculo", testeAreaCirculo)
    , ("testeAreaTriangulo", testeAreaTriangulo)
    , ("testeAreaRetanguloQuadrado", testeAreaRetanguloQuadrado)
    , ("testeVolumeEsfera", testeVolumeEsfera)
    , ("testeVolumeCilindro", testeVolumeCilindro)
    , ("testeVolumeParalelepipedo", testeVolumeParalelepipedo)
    , ("testeVolumeEsferaUnitaria", testeVolumeEsferaUnitaria)
    , ("testeVolumeCilindroUnitario", testeVolumeCilindroUnitario)
    , ("testePerimetroRetangulo", testePerimetroRetangulo)
    , ("testePerimetroCirculo", testePerimetroCirculo)
    , ("testePerimetroCirculoUnitario", testePerimetroCirculoUnitario)
    , ("testePerimetroQuadrado", testePerimetroQuadrado)
    , ("testePerimetroTriangulo", testePerimetroTriangulo)
    , ("testeDentroPoligonoQuadrado", testeDentroPoligonoQuadrado)
    , ("testeDentroPoligonoTriangulo", testeDentroPoligonoTriangulo)
    , ("testeDentroPoligonoBorda", testeDentroPoligonoBorda)
    , ("testeDentroPoligonoVertice", testeDentroPoligonoVertice)
    , ("testeDentroPoligonoVazio", testeDentroPoligonoVazio)
    , ("testeIntersecaoRetasBasico", testeIntersecaoRetasBasico)
    , ("testeIntersecaoRetasParalelas", testeIntersecaoRetasParalelas)
    , ("testeIntersecaoRetasCoincidentes", testeIntersecaoRetasCoincidentes)
    , ("testeIntersecaoRetasPerpendicualres", testeIntersecaoRetasPerpendicualres)
    , ("testeIntersecaoRetasNegativas", testeIntersecaoRetasNegativas)
    ]

-- Executa todos os testes e mostra o resultado
execucaoTestes :: IO ()
execucaoTestes = do
    putStrLn "==== Iniciando Testes de Geometry ===="
    resultados <- mapM executarTeste todosTestes
    let total = length resultados
        passou = length (filter id resultados)
    putStrLn $ "\nResumo final: " ++ show passou ++ "/" ++ show total ++ " testes passaram."

-- Função auxiliar para imprimir cada teste
executarTeste :: (String, Bool) -> IO Bool
executarTeste (nome, resultado) = do
    putStrLn $ nome ++ ": " ++ if resultado then "OK ✅" else "FALHOU ❌"
    return resultado

