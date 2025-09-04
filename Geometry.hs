module Geometry where

import Types (Ponto2D(..), Ponto3D(..), Figura(..), Area, Distancia, Perimetro)


pontoMedio :: Ponto2D -> Ponto2D -> Ponto2D
pontoMedio (Ponto2D x0 y0) (Ponto2D x1 y1) = Ponto2D ((x0+x1)/2) ((y0+y1)/2)
   
-- Funções de distância
distanciaEntrePontos :: Ponto2D -> Ponto2D -> Distancia
distanciaEntrePontos (Ponto2D x0 y0) (Ponto2D x1 y1) =
   sqrt $ (x0-x1)**2 + (y0-y1)**2

distancia3D :: Ponto3D -> Ponto3D -> Distancia
distancia3D (Ponto3D x0 y0 z0) (Ponto3D x1 y1 z1) =
   sqrt $ (x0-x1)**2 + (y0-y1)**2 + (z0-z1)**2
   
-- Funções de área de figuras  
calcularArea :: Figura -> Area

calcularArea (Retangulo l a) = l*a
calcularArea (Circulo r) = pi * r**2
calcularArea (Triangulo (Ponto2D x0 y0) (Ponto2D x1 y1) (Ponto2D x2 y2)) =
   0.5 * (abs $ x0*(y1-y2) + x1*(y2-y0) + x2*(y0-y1))
calcularArea (Esfera r) = 4 * pi * r**2
calcularArea (Cilindro r a) = 2 * pi * r * (r+a)
calcularArea (Paralelepipedo c l a) = 2 * (c*l + c*a + l*a)
calcularArea (Poligono xs) = area
   where
    pareamento = zip xs (tail xs ++ [head xs])
    soma = sum [x0*y1 | (Ponto2D x0 _, Ponto2D _ y1) <- pareamento] - sum [y0*x1 | (Ponto2D _ y0, Ponto2D x1 _) <- pareamento] 
    area = (abs soma) / 2
   
   
-- Funções de perímetro de figuras
calcularPerimetro :: Figura -> Perimetro

calcularPerimetro (Retangulo l a) = 2*l + 2*a
calcularPerimetro (Circulo r) = 2*pi*r
calcularPerimetro (Triangulo p0 p1 p2) = l1 + l2 + l3
   where
    l1 = distanciaEntrePontos p0 p1
    l2 = distanciaEntrePontos p0 p2
    l3 = distanciaEntrePontos p1 p2
calcularPerimetro (Esfera _) = error "Não existe perímetro de figuras 3D"
calcularPerimetro (Cilindro _ _) = error "Não existe perímetro de figuras 3D"
calcularPerimetro (Paralelepipedo _ _ _) = error "Não existe perímetro de figuras 3D"
calcularPerimetro (Poligono xs) = sum [distanciaEntrePontos p0 p1 | (p0,p1) <- pareamento]
   where
    pareamento = zip xs (tail xs ++ [head xs])


