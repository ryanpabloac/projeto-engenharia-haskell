module Geometry where

import Types (Ponto2D(..), Ponto3D(..), Figura(..), Area, Distancia)


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
-- calcularArea (Poligono xs) // Tentar implementar depois




