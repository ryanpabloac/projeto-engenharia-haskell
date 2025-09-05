module Algoritmos where

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