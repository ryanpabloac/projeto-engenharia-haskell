-- 4 funções iniciais => Cadu 
-- falta a função resolverSistemasLineares => Diogo
module LinearAlgebra where

import Types (Angulo, Matriz(..), Vetor(..))












produtoEscalar :: Vetor -> Vetor -> Maybe Double
produtoEscalar _ (Vetor []) = Nothing
produtoEscalar (Vetor xs) (Vetor ys)
    |(length xs) == (length ys) = Just $ sum $ zipWith (*) xs ys
    |otherwise = Nothing

normaVetor :: Vetor -> Double
normaVetor (Vetor []) = 0
normaVetor (Vetor xs) = sqrt $ sum $ map (**2) xs

anguloEntreVetores :: Vetor -> Vetor -> Maybe Angulo
anguloEntreVetores (Vetor xs) (Vetor ys)
    | xs == [] = Nothing
    | length xs /= length ys = Nothing
    | nx == 0 || ny == 0 = Nothing
    | otherwise = Just $ acos $ pe / ((nx) * (ny))
  where
    pe = sum $ zipWith (*) xs ys
    nx = normaVetor (Vetor xs)
    ny = normaVetor (Vetor ys)
