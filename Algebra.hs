-- 4 funções iniciais => Cadu 
module LinearAlgebra where

import Types (Angulo, Matriz(..), Vetor(..))
import Data.Maybe (fromJust)  -- pega uma funcao que retorna Maybe e retorna apenas o que esta na frente do Just















resolverSistemaLinear :: Matriz -> Vetor -> Maybe Vetor -- Usando metodo de cramer
resolverSistemaLinear (Matriz [[]]) _ = Nothing
resolverSistemaLinear (Matriz xss) (Vetor ys) 
    |all (\colunas -> length colunas == length xss) xss =
        if (fromJust (determinante (Matriz xss))) == 0 then Nothing
            else Just (Vetor ([fromJust (determinante(sub (Matriz xss) (Vetor ys) i))/d | i <- [0 .. ((length ys) - 1)]]))
    |otherwise = Nothing
    where d = fromJust (determinante (Matriz xss))
          -- substitui a coluna inteira pelo vetor
          sub (Matriz xss) (Vetor ys) i = Matriz (zipWith (\linha vetorElemento -> take i linha ++ [vetorElemento] ++ drop (i + 1) linha) xss ys)

produtoEscalar :: Vetor -> Vetor -> Maybe Double
produtoEscalar _ (Vetor []) = Nothing
produtoEscalar (Vetor xs) (Vetor ys)
    |(length xs) == (length ys) = Just $ sum $ zipWith (*) xs ys -- o zipWith multiplica cada elemento de um vetor no elemento correnpondente do outro vetor, depois faz a soma.
    |otherwise = Nothing

normaVetor :: Vetor -> Double
normaVetor (Vetor []) = 0
normaVetor (Vetor xs) = sqrt $ sum $ map (**2) xs --o map eleva todos os elementos ao quadrado, depois soma todos os elementos e tira a raiz. 

anguloEntreVetores :: Vetor -> Vetor -> Maybe Angulo -- usando a formula angulo = acos do produto escalar dividido pela multiplicacao das normas dos vetores
anguloEntreVetores (Vetor xs) (Vetor ys)
    | xs == [] = Nothing
    | length xs /= length ys = Nothing
    | nx == 0 || ny == 0 = Nothing
    | otherwise = Just $ acos $ pe / ((nx) * (ny))
  where
    pe = sum $ zipWith (*) xs ys
    nx = normaVetor (Vetor xs)
    ny = normaVetor (Vetor ys)
