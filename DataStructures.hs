module DataStructures where

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
  
