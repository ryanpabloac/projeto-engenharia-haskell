module Geometry where

import Types (Ponto2D(..), Ponto3D(..), Distancia)

distanciaEntrePontos :: Ponto2D -> Ponto2D -> Distancia
distanciaEntrePontos (Ponto2D x0 y0) (Ponto2D x1 y1) =
   sqrt $ (x0-x1)**2 + (y0-y1)**2

distancia3D :: Ponto3D -> Ponto3D -> Distancia
distancia3D (Ponto3D x0 y0 z0) (Ponto3D x1 y1 z1) =
   sqrt $ (x0-x1)**2 + (y0-y1)**2 + (z0-z1)**2
