module Algorithms where

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
    
