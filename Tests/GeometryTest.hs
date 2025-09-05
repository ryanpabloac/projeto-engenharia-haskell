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
