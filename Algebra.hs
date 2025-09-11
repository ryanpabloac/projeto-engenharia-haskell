-- 4 funções iniciais => Cadu 
module LinearAlgebra where

import Types (Angulo, Matriz(..), Vetor(..))
import Data.Maybe (fromJust)  -- pega uma funcao que retorna Maybe e retorna apenas o que esta na frente do Just
import Data.List (transpose) -- pega da biblioteca uma função que faz o processo de transposição de uma matriz.

-- /////////// AUXILIARES ///////////////

tamanho :: Matriz -> (Int, Int) -- (n. linhas, n. colunas)
tamanho (Matriz mat) =
    (length  mat, length (head mat))
    -- 1 > length mat conta as linhas
    -- 2 > length (head mat) conta o numero de elementos, logo, a quantidade de colunas.

verTamanhoIgual :: Matriz -> Matriz -> Bool
verTamanhoIgual (Matriz mat1) (Matriz mat2) =
    tamanho(Matriz mat1) == tamanho(Matriz mat2)
    -- compara (linha.mat1, coluna.mat1) com (linha.mat2, coluna.mat2); 
    
    
-- //////////// PRINCIPAIS ///////////////

somarMatrizes :: Matriz -> Matriz -> Maybe Matriz
somarMatrizes (Matriz mat1) (Matriz mat2)  
    |verTamanhoIgual (Matriz mat1) (Matriz mat2) = Just(Matriz (zipWith (zipWith (+)) mat1 mat2))        -- verifica se as matrizes tem mesmo tamanho e, se tiverem, a soma é feita com o zipwith aninhado que atua sobre cada elemento delas.
    |otherwise = Nothing -- matrizes de ordem diferentes não podem ser somadas. 

transpostaMatriz :: Matriz -> Matriz
transposta (Matriz mat1) =
    Matriz (transpose mat1) -- usa a função importada de Data.List para transpor a matriz.


multiplicarMatrizes :: Matriz -> Matriz -> Maybe Matriz
multiplicarMatrizes (Matriz mat1) (Matriz mat2)
    | (length (head mat1)) == length mat2 = -- se o num. de colunas de mat1 se igualar ao num. de linhas de mat2
        let (Matriz mat2Transp) = transpostaMatriz (Matriz mat2)
        -- transpoe a segunda matriz para facilitar o acesso às colunas.
            mResultante = [[sum (zipWith(*) lMat1 cMat2) -- multiplica os elementos das linhas da primeira matriz com os elementos das colunas da segunda matriz. depois, o "sum" faz a soma dos produtos.
                        | cMat2 <- mat2Transp] 
                        | lMat1 <- mat1]
        in Just (Matriz mResultante) -- resultado da operacao
   | otherwise = Nothing -- as dimensoes das matrizes nao permitem essa operacao

-- FUNCAO DETERMINANTE DANDO ERRO 



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
