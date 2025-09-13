module AlgorithmsTests (executarTestesEspecialista4) where

import Types
import Algorithms
import DataStructures
import Data.Time.Calendar (fromGregorian)

-- =================================
-- TESTES PARA QUICKSORT
-- =================================

testeQuickSortVazio :: Bool
testeQuickSortVazio = quickSort [] == ([] :: [Int])

testeQuickSortUmElemento :: Bool
testeQuickSortUmElemento = quickSort [42] == [42]

testeQuickSortOrdenado :: Bool
testeQuickSortOrdenado =
    let lista = [1, 2, 3, 4, 5]
    in quickSort lista == lista
    
testeQuickSortReverso :: Bool
testeQuickSortReverso =
    let lista = [5, 4, 3, 2, 1]
        esperado = [1, 2, 3, 4, 5]
    in quickSort lista == esperado
    
testeQuickSortDuplicados :: Bool
testeQuickSortDuplicados =
    let lista = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3]
        esperado = [1, 1, 2, 3, 3, 4, 5, 5, 6, 9]
    in quickSort lista == esperado

testeQuickSortNegativos :: Bool
testeQuickSortNegativos =
    let lista = [-5, -1, -10, -3, -7]
        esperado = [-10, -7, -5, -3, -1]
    in quickSort lista == esperado

testeQuickSortMisturado :: Bool
testeQuickSortMisturado =
    let lista = [-2, 5, -8, 0, 3, -1, 7]
        esperado = [-8, -2, -1, 0, 3, 5, 7]
    in quickSort lista == esperado

testeQuickSortTodosIguais :: Bool
testeQuickSortTodosIguais =
    let lista = [7, 7, 7, 7, 7]
    in quickSort lista == lista

testeQuickSortGrande :: Bool
testeQuickSortGrande =
    let lista = [100, 50, 200, 25, 75, 150, 300]
        esperado = [25, 50, 75, 100, 150, 200, 300]
    in quickSort lista == esperado

testeQuickSortDecimal :: Bool
testeQuickSortDecimal =
    let lista = [3.14, 2.71, 1.41, 1.73, 0.57]
        esperado = [0.57, 1.41, 1.73, 2.71, 3.14]
    in quickSort lista == esperado

-- =================================
-- TESTES PARA MERGESORT
-- =================================

testeMergeSortVazio :: Bool
testeMergeSortVazio = mergeSort [] == ([] :: [Int])

testeMergeSortUmElemento :: Bool
testeMergeSortUmElemento = mergeSort [99] == [99]

testeMergeSortCompleto :: Bool
testeMergeSortCompleto =
    let lista = [64, 34, 25, 12, 22, 11, 90]
        resultado = mergeSort lista
        esperado = [11, 12, 22, 25, 34, 64, 90]
    in resultado == esperado

testeMergeSortTodosIguais :: Bool
testeMergeSortTodosIguais =
    let lista = [7, 7, 7, 7, 7]
    in mergeSort lista == lista

testeMergeSortNegativos :: Bool
testeMergeSortNegativos =
    let lista = [-3, -1, -4, -2, -5]
        esperado = [-5, -4, -3, -2, -1]
    in mergeSort lista == esperado

testeMergeSortDoisElementos :: Bool
testeMergeSortDoisElementos =
    let lista = [10, 5]
        esperado = [5, 10]
    in mergeSort lista == esperado

testeMergeSortParImpar :: Bool
testeMergeSortParImpar =
    let lista = [8, 3, 5, 4, 7, 6, 1, 2]
        esperado = [1, 2, 3, 4, 5, 6, 7, 8]
    in mergeSort lista == esperado

testeMergeSortStrings :: Bool
testeMergeSortStrings =
    let lista = ["zebra", "apple", "banana", "cherry"]
        esperado = ["apple", "banana", "cherry", "zebra"]
    in mergeSort lista == esperado

testeMergeSortCrescente :: Bool
testeMergeSortCrescente =
    let lista = [1, 2, 3, 4, 5, 6]
    in mergeSort lista == lista

testeMergeSortDecrescente :: Bool
testeMergeSortDecrescente =
    let lista = [6, 5, 4, 3, 2, 1]
        esperado = [1, 2, 3, 4, 5, 6]
    in mergeSort lista == esperado

-- =================================
-- TESTES PARA INSERTIONSORT
-- =================================

testeInsertionSortVazio :: Bool
testeInsertionSortVazio = insertionSort [] == ([] :: [Int])

testeInsertionSortUmElemento :: Bool
testeInsertionSortUmElemento = insertionSort [15] == [15]

testeInsertionSortCompleto :: Bool
testeInsertionSortCompleto =
    let lista = [64, 34, 25, 12, 22, 11, 90]
        resultado = insertionSort lista
        esperado = [11, 12, 22, 25, 34, 64, 90]
    in resultado == esperado

testeInsertionSortCrescente :: Bool
testeInsertionSortCrescente =
    let lista = [1, 2, 3, 4, 5]
    in insertionSort lista == lista

testeInsertionSortDecimal :: Bool
testeInsertionSortDecimal =
    let lista = [3.14, 2.71, 1.41, 1.73]
        resultado = insertionSort lista
        esperado = [1.41, 1.73, 2.71, 3.14]
    in resultado == esperado

testeInsertionSortDoisElementos :: Bool
testeInsertionSortDoisElementos =
    let lista = [10, 5]
        resultado = insertionSort lista
        esperado = [5, 10]
    in resultado == esperado

testeInsertionSortTresElementos :: Bool
testeInsertionSortTresElementos =
    let lista = [30, 10, 20]
        resultado = insertionSort lista
        esperado = [10, 20, 30]
    in resultado == esperado

testeInsertionSortDuplicados :: Bool
testeInsertionSortDuplicados =
    let lista = [5, 2, 8, 2, 9, 1, 5]
        resultado = insertionSort lista
        esperado = [1, 2, 2, 5, 5, 8, 9]
    in resultado == esperado

testeInsertionSortNegativos :: Bool
testeInsertionSortNegativos =
    let lista = [-1, -5, -3, -2, -4]
        resultado = insertionSort lista
        esperado = [-5, -4, -3, -2, -1]
    in resultado == esperado

testeInsertionSortMisto :: Bool
testeInsertionSortMisto =
    let lista = [-3, 0, 5, -1, 2]
        resultado = insertionSort lista
        esperado = [-3, -1, 0, 2, 5]
    in resultado == esperado


-- =================================
-- TESTES PARA INSERIR ORDENADO
-- =================================

testeInserirOrdenadoMeio :: Bool
testeInserirOrdenadoMeio =
    let lista = [1, 3, 5, 7, 9]
        resultado = inserirOrdenado 4 lista
        esperado = [1, 3, 4, 5, 7, 9]
    in resultado == esperado

testeInserirOrdenadoInicio :: Bool
testeInserirOrdenadoInicio =
    let lista = [2, 4, 6, 8]
        resultado = inserirOrdenado 1 lista
        esperado = [1, 2, 4, 6, 8]
    in resultado == esperado

testeInserirOrdenadoFim :: Bool
testeInserirOrdenadoFim =
    let lista = [1, 3, 5, 7]
        resultado = inserirOrdenado 9 lista
        esperado = [1, 3, 5, 7, 9]
    in resultado == esperado

testeInserirOrdenadoVazio :: Bool
testeInserirOrdenadoVazio =
    let lista = []
        resultado = inserirOrdenado 5 lista
        esperado = [5]
    in resultado == esperado

testeInserirOrdenadoDuplicado :: Bool
testeInserirOrdenadoDuplicado =
    let lista = [1, 3, 5, 7]
        resultado = inserirOrdenado 5 lista
    in length resultado == length lista + 1 && elem 5 resultado

testeInserirOrdenadoNegativo :: Bool
testeInserirOrdenadoNegativo =
    let lista = [-5, -3, -1, 2, 4]
        resultado = inserirOrdenado (-2) lista
        esperado = [-5, -3, -2, -1, 2, 4]
    in resultado == esperado

testeInserirOrdenadoTodosNegativos :: Bool
testeInserirOrdenadoTodosNegativos =
    let lista = [-10, -8, -5, -2]
        resultado = inserirOrdenado (-6) lista
        esperado = [-10, -8, -6, -5, -2]
    in resultado == esperado

testeInserirOrdenadoDecimal :: Bool
testeInserirOrdenadoDecimal =
    let lista = [1.1, 2.2, 3.3, 4.4]
        resultado = inserirOrdenado 2.7 lista
        esperado = [1.1, 2.2, 2.7, 3.3, 4.4]
    in resultado == esperado

testeInserirOrdenadoUmElemento :: Bool
testeInserirOrdenadoUmElemento =
    let lista = [10]
        resultadoMenor = inserirOrdenado 5 lista
        resultadoMaior = inserirOrdenado 15 lista
    in resultadoMenor == [5, 10] && resultadoMaior == [10, 15]

testeInserirOrdenadoString :: Bool
testeInserirOrdenadoString =
    let lista = ["apple", "cherry", "orange"]
        resultado = inserirOrdenado "banana" lista
        esperado = ["apple", "banana", "cherry", "orange"]
    in resultado == esperado

-- =================================
-- FUNçõES DE TESTE AUXILIARES
-- =================================

executarTestesEspecialista4 :: IO ()
executarTestesEspecialista4 = do
    putStrLn "======================================"
    putStrLn " TESTES ESPECIALISTA 4"
    putStrLn " (Algoritmos e Estruturas)"
    putStrLn "======================================"

    putStrLn "\n-- TESTES QUICKSORT --"
    putStrLn $ "Vazio: " ++ show testeQuickSortVazio
    putStrLn $ "Um elemento: " ++ show testeQuickSortUmElemento
    putStrLn $ "Ordenado: " ++ show testeQuickSortOrdenado
    putStrLn $ "Reverso: " ++ show testeQuickSortReverso
    putStrLn $ "Duplicados: " ++ show testeQuickSortDuplicados
    putStrLn $ "Negativos: " ++ show testeQuickSortNegativos
    putStrLn $ "Misturado: " ++ show testeQuickSortMisturado
    putStrLn $ "Todos iguais: " ++ show testeQuickSortTodosIguais
    putStrLn $ "Grande: " ++ show testeQuickSortGrande
    putStrLn $ "Decimal: " ++ show testeQuickSortDecimal

    putStrLn "\n-- TESTES MERGESORT --"
    putStrLn $ "Vazio: " ++ show testeMergeSortVazio
    putStrLn $ "Um elemento: " ++ show testeMergeSortUmElemento
    putStrLn $ "Completo: " ++ show testeMergeSortCompleto
    putStrLn $ "Todos iguais: " ++ show testeMergeSortTodosIguais
    putStrLn $ "Negativos: " ++ show testeMergeSortNegativos
    putStrLn $ "Dois elementos: " ++ show testeMergeSortDoisElementos
    putStrLn $ "Par/´Impar: " ++ show testeMergeSortParImpar
    putStrLn $ "Strings: " ++ show testeMergeSortStrings
    putStrLn $ "Crescente: " ++ show testeMergeSortCrescente
    putStrLn $ "Decrescente: " ++ show testeMergeSortDecrescente

    putStrLn "\n-- TESTES INSERTIONSORT --"
    putStrLn $ "Vazio: " ++ show testeInsertionSortVazio
    putStrLn $ "Um elemento: " ++ show testeInsertionSortUmElemento
    putStrLn $ "Completo: " ++ show testeInsertionSortCompleto
    putStrLn $ "Crescente: " ++ show testeInsertionSortCrescente
    putStrLn $ "Decimal: " ++ show testeInsertionSortDecimal
    putStrLn $ "Dois elementos: " ++ show testeInsertionSortDoisElementos
    putStrLn $ "Tr^es elementos: " ++ show testeInsertionSortTresElementos
    putStrLn $ "Duplicados: " ++ show testeInsertionSortDuplicados
    putStrLn $ "Negativos: " ++ show testeInsertionSortNegativos
    putStrLn $ "Misto: " ++ show testeInsertionSortMisto

    putStrLn "\n-- TESTES INSERIR ORDENADO --"
    putStrLn $ "Meio: " ++ show testeInserirOrdenadoMeio
    putStrLn $ "In´ıcio: " ++ show testeInserirOrdenadoInicio
    putStrLn $ "Fim: " ++ show testeInserirOrdenadoFim
    putStrLn $ "Vazio: " ++ show testeInserirOrdenadoVazio
    putStrLn $ "Duplicado: " ++ show testeInserirOrdenadoDuplicado
    putStrLn $ "Negativo: " ++ show testeInserirOrdenadoNegativo
    putStrLn $ "Todos negativos: " ++ show testeInserirOrdenadoTodosNegativos
    putStrLn $ "Decimal: " ++ show testeInserirOrdenadoDecimal
    putStrLn $ "Um elemento: " ++ show testeInserirOrdenadoUmElemento
    putStrLn $ "String: " ++ show testeInserirOrdenadoString

testesEspecialista4 :: [(String, Bool)]
testesEspecialista4 =
    [ -- QuickSort
        ("QuickSort Vazio", testeQuickSortVazio)
        , ("QuickSort Um Elemento", testeQuickSortUmElemento)
        , ("QuickSort Ordenado", testeQuickSortOrdenado)
        , ("QuickSort Reverso", testeQuickSortReverso)
        , ("QuickSort Duplicados", testeQuickSortDuplicados)
        , ("QuickSort Negativos", testeQuickSortNegativos)
        , ("QuickSort Misturado", testeQuickSortMisturado)
        , ("QuickSort Todos Iguais", testeQuickSortTodosIguais)
        , ("QuickSort Grande", testeQuickSortGrande)
        , ("QuickSort Decimal", testeQuickSortDecimal)

        -- MergeSort
        , ("MergeSort Vazio", testeMergeSortVazio)
        , ("MergeSort Um Elemento", testeMergeSortUmElemento)
        , ("MergeSort Completo", testeMergeSortCompleto)
        , ("MergeSort Todos Iguais", testeMergeSortTodosIguais)
        , ("MergeSort Negativos", testeMergeSortNegativos)
        , ("MergeSort Dois Elementos", testeMergeSortDoisElementos)
        , ("MergeSort Par/´Impar", testeMergeSortParImpar)
        , ("MergeSort Strings", testeMergeSortStrings)
        , ("MergeSort Crescente", testeMergeSortCrescente)
        , ("MergeSort Decrescente", testeMergeSortDecrescente)

        -- InsertionSort
        , ("InsertionSort Vazio", testeInsertionSortVazio)
        , ("InsertionSort Um Elemento", testeInsertionSortUmElemento)
        , ("InsertionSort Completo", testeInsertionSortCompleto)
        , ("InsertionSort Crescente", testeInsertionSortCrescente)
        , ("InsertionSort Decimal", testeInsertionSortDecimal)
        , ("InsertionSort Dois Elementos", testeInsertionSortDoisElementos)
        , ("InsertionSort Tr^es Elementos", testeInsertionSortTresElementos)
        , ("InsertionSort Duplicados", testeInsertionSortDuplicados)
        , ("InsertionSort Negativos", testeInsertionSortNegativos)
        , ("InsertionSort Misto", testeInsertionSortMisto)

        -- Inserir Ordenado
        , ("Inserir Ordenado Meio", testeInserirOrdenadoMeio)
        , ("Inserir Ordenado In´ıcio", testeInserirOrdenadoInicio)
        , ("Inserir Ordenado Fim", testeInserirOrdenadoFim)
        , ("Inserir Ordenado Vazio", testeInserirOrdenadoVazio)
        , ("Inserir Ordenado Duplicado", testeInserirOrdenadoDuplicado)
    ]
