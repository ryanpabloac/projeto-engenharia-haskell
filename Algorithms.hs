module Algoritmos where

-- função merge sort (dividir e conquistar)
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs | metade >= 1 = merge (mergeSort começo) (mergeSort final)
             | otherwise = xs
             where
                metade = length xs `div` 2
                começo = take metade xs
                final = drop metade xs

merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
    |x <= y = x : merge xs (y : ys)
    | x > y = y : merge (x : xs) ys 


-- função quick sort (baseado no pivó)
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort menor ++ [x] ++ quickSort maior
    where
        menor = [y | y <- xs, y < x]
        maior = [y | y <- xs, y >= x]

-- função insertion sort (destruir e reconstruir)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = inserir x (insertionSort xs)
  where
    inserir y [] = [y]
    inserir y (z:zs)
      | y <= z = y:z:zs
      | otherwise = z : inserir y zs

-- função inserir ordenado

inserirOrdenado :: (Ord a) => a -> [a] -> [a]
inserirOrdenado y [] = [y]
inserirOrdenado y (z:zs)
    | y <= z = y:z:zs
    | otherwise = z : inserirOrdenado y zs

-- estrutura de dados árvore binária

data ArvoreBinaria a = Vazia | No a (ArvoreBinaria a) (ArvoreBinaria a)
    deriving (Show, Eq)

-- Função construir a árvore a partir da lista (quando usar quarda a arvore)
construirArvore :: (Ord a) => [a] -> ArvoreBinaria a
construirArvore = foldr inserirElemento Vazia

-- Função auxiliar para ciar os nos da arvore
inserirElemento :: (Ord a) => a -> ArvoreBinaria a -> ArvoreBinaria a
inserirElemento x Vazia = No x Vazia Vazia
inserirElemento x (No y esq dir)
    | x < y = No y (inserirElemento x esq) dir
    | x > y = No y esq (inserirElemento x dir)
    | otherwise = No y esq dir -- Caso de elemento duplicado, não faz nada

-- Função para buscar um elemento na árvore
buscarArvore :: (Ord a) => a -> ArvoreBinaria a -> Bool
buscarArvore _ Vazia = False
buscarArvore x (No y esq dir)
    | x == y = True
    | x < y  = buscarArvore x esq
    | otherwise = buscarArvore x dir
