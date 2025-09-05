module Tests.SortingTests where

import Types
import Algorithms
import Data.List (sort, nub)

-- =================================
-- TESTES PARA QUICKSORT
-- =================================

-- Casos básicos
testeQuickSortVazio :: Bool
testeQuickSortVazio = quickSort ([] :: [Int]) == []

testeQuickSortUmElemento :: Bool
testeQuickSortUmElemento = quickSort [42] == [42]

testeQuickSortDoisElementos :: Bool
testeQuickSortDoisElementos = quickSort [10, 5] == [5, 10]

testeQuickSortTresElementos :: Bool
testeQuickSortTresElementos = quickSort [3, 1, 2] == [1, 2, 3]

-- Listas j´a ordenadas
testeQuickSortOrdenado :: Bool
testeQuickSortOrdenado = quickSort [1, 2, 3, 4, 5] == [1, 2, 3, 4, 5]

testeQuickSortOrdenadoGrande :: Bool
testeQuickSortOrdenadoGrande = quickSort [1..20] == [1..20]

-- Listas em ordem reversa
testeQuickSortInverso :: Bool
testeQuickSortInverso = quickSort [5, 4, 3, 2, 1] == [1, 2, 3, 4, 5]

testeQuickSortInversoGrande :: Bool
testeQuickSortInversoGrande = quickSort [20, 19..1] == [1..20]

-- Elementos duplicados
testeQuickSortDuplicados :: Bool
testeQuickSortDuplicados = quickSort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3] == [1, 1, 2, 3, 3, 4, testeQuickSortTodosDuplicados :: Bool
testeQuickSortTodosDuplicados = quickSort [7, 7, 7, 7, 7] == [7, 7, 7, 7, 7]

testeQuickSortMuitasDuplicatas :: Bool
testeQuickSortMuitasDuplicatas =
    let lista = [1, 2, 1, 3, 2, 1, 4, 3, 2, 1]
        esperado = [1, 1, 1, 1, 2, 2, 2, 3, 3, 4]
    in quickSort lista == esperado

-- N´umeros negativos
testeQuickSortNegativos :: Bool
testeQuickSortNegativos = quickSort [-5, -1, -10, -3, -7] == [-10, -7, -5, -3, -1]

testeQuickSortMistoPositivoNegativo :: Bool
testeQuickSortMistoPositivoNegativo =
    quickSort [-3, 0, 5, -1, 2, -7, 10] == [-7, -3, -1, 0, 2, 5, 10]

-- Diferentes tipos
testeQuickSortStrings :: Bool
testeQuickSortStrings =
    quickSort ["zebra", "apple", "banana", "cherry"] == ["apple", "banana", "cherry", "zebra"]

testeQuickSortCaracteres :: Bool
testeQuickSortCaracteres = quickSort ['d', 'a', 'c', 'b'] == ['a', 'b', 'c', 'd']

testeQuickSortDecimais :: Bool
testeQuickSortDecimais =
    quickSort [3.14, 2.71, 1.41, 1.73] == [1.41, 1.73, 2.71, 3.14]

-- Casos extremos
testeQuickSortGrande :: Bool
testeQuickSortGrande =
    let lista = [50, 25, 75, 12, 37, 62, 87, 6, 18, 31, 43, 56, 68, 81, 93]
        resultado = quickSort lista
        esperado = [6, 12, 18, 25, 31, 37, 43, 50, 56, 62, 68, 75, 81, 87, 93]
    in resultado == esperado

testeQuickSortValoresExtremos :: Bool
testeQuickSortValoresExtremos =
    let maxInt = 2147483647
        minInt = -2147483648
        lista = [maxInt, 0, minInt, 1, -1]
        esperado = [minInt, -1, 0, 1, maxInt]
    in quickSort lista == esperado

-- Listas com padr~oes espec´ıficos
testeQuickSortPadrao :: Bool
testeQuickSortPadrao =
    let lista = [7, 2, 9, 1, 5, 3, 8, 4, 6]
        esperado = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    in quickSort lista == esperado

testeQuickSortFibonacci :: Bool
testeQuickSortFibonacci =
    let fibonacci = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
        fibMisturado = [21, 1, 55, 3, 13, 1, 8, 34, 2, 5]
    in quickSort fibMisturado == fibonacci

-- =================================
-- TESTES PARA MERGESORT
-- =================================

-- Casos b´asicos
testeMergeSortVazio :: Bool
testeMergeSortVazio = mergeSort ([] :: [Int]) == []

testeMergeSortUmElemento :: Bool
testeMergeSortUmElemento = mergeSort [42] == [42]

testeMergeSortDoisElementos :: Bool
testeMergeSortDoisElementos = mergeSort [10, 5] == [5, 10]

testeMergeSortTresElementos :: Bool
testeMergeSortTresElementos = mergeSort [3, 1, 2] == [1, 2, 3]

-- Listas j´a ordenadas
testeMergeSortOrdenado :: Bool
testeMergeSortOrdenado = mergeSort [1, 2, 3, 4, 5] == [1, 2, 3, 4, 5]

testeMergeSortOrdenadoPar :: Bool
testeMergeSortOrdenadoPar = mergeSort [2, 4, 6, 8] == [2, 4, 6, 8]

testeMergeSortOrdenadoImpar :: Bool
testeMergeSortOrdenadoImpar = mergeSort [1, 3, 5, 7, 9] == [1, 3, 5, 7, 9]

-- Listas em ordem reversa
testeMergeSortInverso :: Bool
testeMergeSortInverso = mergeSort [5, 4, 3, 2, 1] == [1, 2, 3, 4, 5]

testeMergeSortInversoPar :: Bool
testeMergeSortInversoPar = mergeSort [8, 6, 4, 2] == [2, 4, 6, 8]

-- Elementos duplicados
testeMergeSortDuplicados :: Bool
testeMergeSortDuplicados = mergeSort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3] == [1, 1, 2, 3, 3, 4, 5, 5, 6,9]

testeMergeSortTodosDuplicados :: Bool
testeMergeSortTodosDuplicados = mergeSort [4, 4, 4, 4] == [4, 4, 4, 4]

testeMergeSortDuplicatasComplexas :: Bool
testeMergeSortDuplicatasComplexas =
    let lista = [5, 1, 5, 2, 1, 5, 3, 2, 1]
        esperado = [1, 1, 1, 2, 2, 3, 5, 5, 5]
    in mergeSort lista == esperado

-- N´umeros negativos
testeMergeSortNegativos :: Bool
testeMergeSortNegativos = mergeSort [-3, -1, -4, -2, -5] == [-5, -4, -3, -2, -1]

testeMergeSortMistoSignos :: Bool
testeMergeSortMistoSignos =
    mergeSort [-2, 5, -8, 0, 3, -1, 7] == [-8, -2, -1, 0, 3, 5, 7]

-- Diferentes tipos
testeMergeSortStrings :: Bool
testeMergeSortStrings =
    mergeSort ["delta", "alpha", "gamma", "beta"] == ["alpha", "beta", "delta", "gamma"]

testeMergeSortCaracteres :: Bool
testeMergeSortCaracteres = mergeSort ['z', 'a', 'm', 'c'] == ['a', 'c', 'm', 'z']

testeMergeSortDecimais :: Bool
testeMergeSortDecimais =
    mergeSort [2.71, 3.14, 1.41, 1.73] == [1.41, 1.73, 2.71, 3.14]

-- Casos de tamanho espec´ıfico
testeMergeSortTamanho4 :: Bool
testeMergeSortTamanho4 = mergeSort [4, 2, 3, 1] == [1, 2, 3, 4]

testeMergeSortTamanho8 :: Bool
testeMergeSortTamanho8 =
    mergeSort [8, 4, 12, 2, 10, 6, 14, 1] == [1, 2, 4, 6, 8, 10, 12, 14]

testeMergeSortTamanho16 :: Bool
testeMergeSortTamanho16 =
    let lista = [16, 8, 24, 4, 20, 12, 28, 2, 18, 10, 26, 6, 22, 14, 30, 1]
        esperado = [1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30]
    in mergeSort lista == esperado

-- Performance e estabilidade
testeMergeSortGrande :: Bool
testeMergeSortGrande =
    let lista = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
        esperado = [11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90]
    in mergeSort lista == esperado

testeMergeSortEstabilidade :: Bool
testeMergeSortEstabilidade =
    -- Para testar estabilidade, usamos tuplas onde o primeiro elemento ´e chave
    let lista = [(3, 'a'), (1, 'b'), (3, 'c'), (2, 'd'), (1, 'e')]
        resultado = mergeSort lista
        esperado = [(1, 'b'), (1, 'e'), (2, 'd'), (3, 'a'), (3, 'c')]
    in resultado == esperado

-- =================================
-- TESTES PARA INSERTIONSORT
-- =================================

-- Casos b´asicos
testeInsertionSortVazio :: Bool
testeInsertionSortVazio = insertionSort ([] :: [Int]) == []

testeInsertionSortUmElemento :: Bool
testeInsertionSortUmElemento = insertionSort [42] == [42]

testeInsertionSortDoisElementos :: Bool
testeInsertionSortDoisElementos = insertionSort [20, 10] == [10, 20]

testeInsertionSortDoisElementosOrdenados :: Bool
testeInsertionSortDoisElementosOrdenados = insertionSort [10, 20] == [10, 20]

-- Listas pequenas
testeInsertionSortTresElementos :: Bool
testeInsertionSortTresElementos = insertionSort [3, 1, 2] == [1, 2, 3]

testeInsertionSortQuatroElementos :: Bool
testeInsertionSortQuatroElementos = insertionSort [4, 2, 3, 1] == [1, 2, 3, 4]

testeInsertionSortCincoElementos :: Bool
testeInsertionSortCincoElementos = insertionSort [5, 2, 4, 1, 3] == [1, 2, 3, 4, 5]

-- Listas j´a ordenadas
testeInsertionSortOrdenado :: Bool
testeInsertionSortOrdenado = insertionSort [1, 2, 3, 4, 5] == [1, 2, 3, 4, 5]

testeInsertionSortOrdenadoMedio :: Bool
testeInsertionSortOrdenadoMedio =
insertionSort [1, 3, 5, 7, 9, 11, 13] == [1, 3, 5, 7, 9, 11, 13]

-- Listas em ordem reversa
testeInsertionSortInverso :: Bool
testeInsertionSortInverso = insertionSort [5, 4, 3, 2, 1] == [1, 2, 3, 4, 5]

testeInsertionSortInversoGrande :: Bool
testeInsertionSortInversoGrande =
    insertionSort [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-- Elementos duplicados
testeInsertionSortDuplicados :: Bool
testeInsertionSortDuplicados =
    insertionSort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3] == [1, 1, 2, 3, 3, 4, 5, 5, 6, 9]

testeInsertionSortTodosDuplicados :: Bool
testeInsertionSortTodosDuplicados =
    insertionSort [6, 6, 6, 6, 6] == [6, 6, 6, 6, 6]

testeInsertionSortDuplicatasParciais :: Bool
testeInsertionSortDuplicatasParciais =
    insertionSort [2, 1, 2, 3, 1, 3] == [1, 1, 2, 2, 3, 3]

-- N´umeros negativos
testeInsertionSortNegativos :: Bool
testeInsertionSortNegativos =
    insertionSort [-1, -5, -3, -2, -4] == [-5, -4, -3, -2, -1]

testeInsertionSortMistoPositivoNegativo :: Bool
testeInsertionSortMistoPositivoNegativo =
    insertionSort [-1, 3, -2, 0, 1, -3, 2] == [-3, -2, -1, 0, 1, 2, 3]

testeInsertionSortComZero :: Bool
testeInsertionSortComZero =
    insertionSort [0, -1, 1, -2, 2] == [-2, -1, 0, 1, 2]

-- Diferentes tipos de dados
testeInsertionSortStrings :: Bool
testeInsertionSortStrings =
    insertionSort ["echo", "alpha", "delta", "beta", "charlie"] ==
    ["alpha", "beta", "charlie", "delta", "echo"]

testeInsertionSortCaracteres :: Bool
testeInsertionSortCaracteres =
    insertionSort ['m', 'a', 't', 'h'] == ['a', 'h', 'm', 't']

testeInsertionSortDecimais :: Bool
testeInsertionSortDecimais =
    insertionSort [0.5, 0.1, 0.9, 0.3, 0.7] == [0.1, 0.3, 0.5, 0.7, 0.9]

-- Casos de melhor/pior cen´ario para insertion sort
testeInsertionSortMelhorCaso :: Bool
testeInsertionSortMelhorCaso =
    -- Lista quase ordenada (melhor caso para insertion sort)
    insertionSort [1, 2, 3, 5, 4] == [1, 2, 3, 4, 5]

testeInsertionSortPiorCaso :: Bool
testeInsertionSortPiorCaso =
    -- Lista totalmente reversa (pior caso)
    insertionSort [7, 6, 5, 4, 3, 2, 1] == [1, 2, 3, 4, 5, 6, 7]

testeInsertionSortCasoMedio :: Bool
testeInsertionSortCasoMedio =
    insertionSort [4, 2, 7, 1, 9, 3, 6, 5, 8] == [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- =================================
-- TESTES DE Consistência ENTRE ALGORITMOS
-- =================================

testeConsistencia1 :: Bool
testeConsistencia1 =
    let lista = [7, 2, 9, 1, 5, 3, 8, 4, 6]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
    in quick == merge && merge == insertion

testeConsistencia2 :: Bool
testeConsistencia2 =
    let lista = [64, 34, 25, 12, 22, 11, 90]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
        esperado = [11, 12, 22, 25, 34, 64, 90]
    in quick == esperado && merge == esperado && insertion == esperado

testeConsistenciaVazio :: Bool
testeConsistenciaVazio =
    let lista = [] :: [Int]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
    in quick == merge && merge == insertion && quick == []

testeConsistenciaUmElemento :: Bool
testeConsistenciaUmElemento =
    let lista = [99]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
    in quick == merge && merge == insertion && quick == lista

testeConsistenciaDuplicados :: Bool
testeConsistenciaDuplicados =
    let lista = [5, 2, 8, 2, 9, 1, 5, 1]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
        esperado = [1, 1, 2, 2, 5, 5, 8, 9]
    in quick == esperado && merge == esperado && insertion == esperado

testeConsistenciaNegativos :: Bool
testeConsistenciaNegativos =
    let lista = [-10, 5, -3, 8, -1, 0, 2]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
        esperado = [-10, -3, -1, 0, 2, 5, 8]
    in quick == esperado && merge == esperado && insertion == esperado

testeConsistenciaStrings :: Bool
testeConsistenciaStrings =
    let lista = ["zebra", "apple", "banana", "cherry", "date"]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
        esperado = ["apple", "banana", "cherry", "date", "zebra"]
    in quick == esperado && merge == esperado && insertion == esperado

testeConsistenciaDecimais :: Bool
testeConsistenciaDecimais =
    let lista = [3.14, 2.71, 1.41, 1.73, 0.57]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
        esperado = [0.57, 1.41, 1.73, 2.71, 3.14]
    in quick == esperado && merge == esperado && insertion == esperado

testeConsistenciaGrande :: Bool
testeConsistenciaGrande =
    let lista = [i `mod` 50 | i <- [1..100]] -- Lista com muitas duplicatas
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
    in quick == merge && merge == insertion

testeConsistenciaCaracteres :: Bool
testeConsistenciaCaracteres =
    let lista = ['h', 'a', 's', 'k', 'e', 'l', 'l']
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
        esperado = ['a', 'e', 'h', 'k', 'l', 'l', 's']
    in quick == esperado && merge == esperado && insertion == esperado

testeConsistenciaValoresExtremos :: Bool
testeConsistenciaValoresExtremos =
    let maxInt = 1000000
        minInt = -1000000
        lista = [maxInt, 0, minInt, 1, -1, 100, -100]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
        esperado = [minInt, -100, -1, 0, 1, 100, maxInt]
    in quick == esperado && merge == esperado && insertion == esperado

-- =================================
-- TESTES DE PROPRIEDADES
-- =================================

testePropriedadeComprimento :: Bool
testePropriedadeComprimento =
    let lista = [5, 2, 8, 1, 9, 3, 7, 4, 6]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
    in length quick == length lista &&
        length merge == length lista &&
        length insertion == length lista

testePropriedadeElementos :: Bool
testePropriedadeElementos =
    let lista = [5, 2, 8, 1, 9, 3, 7, 4, 6]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
        contemTodos xs = all (`elem` xs) lista
    in contemTodos quick && contemTodos merge && contemTodos insertion

testePropriedadeOrdem :: Bool
testePropriedadeOrdem =
    let lista = [5, 2, 8, 1, 9, 3, 7, 4, 6]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
        estaOrdenado [] = True
        estaOrdenado [_] = True
        estaOrdenado (x:y:xs) = x <= y && estaOrdenado (y:xs)
    in estaOrdenado quick && estaOrdenado merge && estaOrdenado insertion

testePropriedadeIdempotencia :: Bool
testePropriedadeIdempotencia =
    let lista = [5, 2, 8, 1, 9]
        quick1 = quickSort lista
        quick2 = quickSort quick1
        merge1 = mergeSort lista
        merge2 = mergeSort merge1
        insertion1 = insertionSort lista
        insertion2 = insertionSort insertion1
    in quick1 == quick2 && merge1 == merge2 && insertion1 == insertion2

testePropriedadeListaVazia :: Bool
testePropriedadeListaVazia =
    let vazia = [] :: [Int]
    in quickSort vazia == vazia &&
        mergeSort vazia == vazia &&
        insertionSort vazia == vazia

-- =================================
-- FUNÇÃO DE EXECUÇÃO DOS TESTES
-- =================================

executarTestesOrdenacao :: IO ()
executarTestesOrdenacao = do
    putStrLn "======================================"
    putStrLn " TESTES ALGORITMOS DE ORDENAÇÃO"
    putStrLn "======================================"

    putStrLn "\n-- TESTES QUICKSORT --"
    putStrLn $ "Vazio: " ++ show testeQuickSortVazio
    putStrLn $ "Um elemento: " ++ show testeQuickSortUmElemento
    putStrLn $ "Dois elementos: " ++ show testeQuickSortDoisElementos
    putStrLn $ "Três elementos: " ++ show testeQuickSortTresElementos
    putStrLn $ "Ordenado: " ++ show testeQuickSortOrdenado
    putStrLn $ "Ordenado grande: " ++ show testeQuickSortOrdenadoGrande
    putStrLn $ "Inverso: " ++ show testeQuickSortInverso
    putStrLn $ "Inverso grande: " ++ show testeQuickSortInversoGrande
    putStrLn $ "Duplicados: " ++ show testeQuickSortDuplicados
    putStrLn $ "Todos duplicados: " ++ show testeQuickSortTodosDuplicados
    putStrLn $ "Muitas duplicatas: " ++ show testeQuickSortMuitasDuplicatas
    putStrLn $ "Negativos: " ++ show testeQuickSortNegativos
    putStrLn $ "Misto +/-: " ++ show testeQuickSortMistoPositivoNegativo
    putStrLn $ "Strings: " ++ show testeQuickSortStrings
    putStrLn $ "Caracteres: " ++ show testeQuickSortCaracteres
    putStrLn $ "Decimais: " ++ show testeQuickSortDecimais
    putStrLn $ "Grande: " ++ show testeQuickSortGrande
    putStrLn $ "Valores extremos: " ++ show testeQuickSortValoresExtremos
    putStrLn $ "Padrão: " ++ show testeQuickSortPadrao
    putStrLn $ "Fibonacci: " ++ show testeQuickSortFibonacci

    putStrLn "\n-- TESTES MERGESORT --"
    putStrLn $ "Vazio: " ++ show testeMergeSortVazio
    putStrLn $ "Um elemento: " ++ show testeMergeSortUmElemento
    putStrLn $ "Dois elementos: " ++ show testeMergeSortDoisElementos
    putStrLn $ "Três elementos: " ++ show testeMergeSortTresElementos
    putStrLn $ "Ordenado: " ++ show testeMergeSortOrdenado
    putStrLn $ "Ordenado par: " ++ show testeMergeSortOrdenadoPar
    putStrLn $ "Ordenado ímpar: " ++ show testeMergeSortOrdenadoImpar
    putStrLn $ "Inverso: " ++ show testeMergeSortInverso
    putStrLn $ "Inverso par: " ++ show testeMergeSortInversoPar
    putStrLn $ "Duplicados: " ++ show testeMergeSortDuplicados
    putStrLn $ "Todos duplicados: " ++ show testeMergeSortTodosDuplicados
    putStrLn $ "Duplicatas complexas: " ++ show testeMergeSortDuplicatasComplexas
    putStrLn $ "Negativos: " ++ show testeMergeSortNegativos
    putStrLn $ "Misto sinais: " ++ show testeMergeSortMistoSignos
    putStrLn $ "Strings: " ++ show testeMergeSortStrings
    putStrLn $ "Caracteres: " ++ show testeMergeSortCaracteres
    putStrLn $ "Decimais: " ++ show testeMergeSortDecimais
    putStrLn $ "Tamanho 4: " ++ show testeMergeSortTamanho4
    putStrLn $ "Tamanho 8: " ++ show testeMergeSortTamanho8
    putStrLn $ "Tamanho 16: " ++ show testeMergeSortTamanho16
    putStrLn $ "Grande: " ++ show testeMergeSortGrande
    putStrLn $ "Estabilidade: " ++ show testeMergeSortEstabilidade

    putStrLn "\n-- TESTES INSERTIONSORT --"
    putStrLn $ "Vazio: " ++ show testeInsertionSortVazio
    putStrLn $ "Um elemento: " ++ show testeInsertionSortUmElemento
    putStrLn $ "Dois elementos: " ++ show testeInsertionSortDoisElementos
    putStrLn $ "Dois ordenados: " ++ show testeInsertionSortDoisElementosOrdenados
    putStrLn $ "Três elementos: " ++ show testeInsertionSortTresElementos
    putStrLn $ "Quatro elementos: " ++ show testeInsertionSortQuatroElementos
    putStrLn $ "Cinco elementos: " ++ show testeInsertionSortCincoElementos
    putStrLn $ "Ordenado: " ++ show testeInsertionSortOrdenado
    putStrLn $ "Ordenado médio: " ++ show testeInsertionSortOrdenadoMedio
    putStrLn $ "Inverso: " ++ show testeInsertionSortInverso
    putStrLn $ "Inverso grande: " ++ show testeInsertionSortInversoGrande
    putStrLn $ "Duplicados: " ++ show testeInsertionSortDuplicados
    putStrLn $ "Todos duplicados: " ++ show testeInsertionSortTodosDuplicados
    putStrLn $ "Duplicatas parciais: " ++ show testeInsertionSortDuplicatasParciais
    putStrLn $ "Negativos: " ++ show testeInsertionSortNegativos
    putStrLn $ "Misto +/-: " ++ show testeInsertionSortMistoPositivoNegativo
    putStrLn $ "Com zero: " ++ show testeInsertionSortComZero
    putStrLn $ "Strings: " ++ show testeInsertionSortStrings
    putStrLn $ "Caracteres: " ++ show testeInsertionSortCaracteres
    putStrLn $ "Decimais: " ++ show testeInsertionSortDecimais
    putStrLn $ "Melhor caso: " ++ show testeInsertionSortMelhorCaso
    putStrLn $ "Pior caso: " ++ show testeInsertionSortPiorCaso
    putStrLn $ "Caso médio: " ++ show testeInsertionSortCasoMedio

    putStrLn "\n-- TESTES DE CONSISTÊNCIA --"
    putStrLn $ "Consistência 1: " ++ show testeConsistencia1
    putStrLn $ "Consistência 2: " ++ show testeConsistencia2
    putStrLn $ "Consistência vazio: " ++ show testeConsistenciaVazio
    putStrLn $ "Consistência um elemento: " ++ show testeConsistenciaUmElemento
    putStrLn $ "Consistência duplicados: " ++ show testeConsistenciaDuplicados
    putStrLn $ "Consistência negativos: " ++ show testeConsistenciaNegativos
    putStrLn $ "Consistência strings: " ++ show testeConsistenciaStrings
    putStrLn $ "Consistência decimais: " ++ show testeConsistenciaDecimais
    putStrLn $ "Consistência grande: " ++ show testeConsistenciaGrande
    putStrLn $ "Consistência caracteres: " ++ show testeConsistenciaCaracteres
    putStrLn $ "Consistência extremos: " ++ show testeConsistenciaValoresExtremos

    putStrLn "\n-- TESTES DE PROPRIEDADES --"
    putStrLn $ "Propriedade comprimento: " ++ show testePropriedadeComprimento
    putStrLn $ "Propriedade elementos: " ++ show testePropriedadeElementos
    putStrLn $ "Propriedade ordem: " ++ show testePropriedadeOrdem
    putStrLn $ "Propriedade idempotência: " ++ show testePropriedadeIdempotencia
    putStrLn $ "Propriedade lista vazia: " ++ show testePropriedadeListaVazia

testesAlgoritmosOrdenacao :: [(String, Bool)]
testesAlgoritmosOrdenacao =
    [ -- QuickSort (20 testes)
        ("QuickSort Vazio", testeQuickSortVazio)
    , ("QuickSort Um Elemento", testeQuickSortUmElemento)
    , ("QuickSort Dois Elementos", testeQuickSortDoisElementos)
    , ("QuickSort Tr^es Elementos", testeQuickSortTresElementos)
    , ("QuickSort Ordenado", testeQuickSortOrdenado)
    , ("QuickSort Ordenado Grande", testeQuickSortOrdenadoGrande)
    , ("QuickSort Inverso", testeQuickSortInverso)
    , ("QuickSort Inverso Grande", testeQuickSortInversoGrande)
    , ("QuickSort Duplicados", testeQuickSortDuplicados)
    , ("QuickSort Todos Duplicados", testeQuickSortTodosDuplicados)
    , ("QuickSort Muitas Duplicatas", testeQuickSortMuitasDuplicatas)
    , ("QuickSort Negativos", testeQuickSortNegativos)
    , ("QuickSort Misto +/-", testeQuickSortMistoPositivoNegativo)
    , ("QuickSort Strings", testeQuickSortStrings)
    , ("QuickSort Caracteres", testeQuickSortCaracteres)
    , ("QuickSort Decimais", testeQuickSortDecimais)
    , ("QuickSort Grande", testeQuickSortGrande)
    , ("QuickSort Valores Extremos", testeQuickSortValoresExtremos)
    , ("QuickSort Padr~ao", testeQuickSortPadrao)
    , ("QuickSort Fibonacci", testeQuickSortFibonacci)

    -- MergeSort (20 testes)
    , ("MergeSort Vazio", testeMergeSortVazio)
    , ("MergeSort Um Elemento", testeMergeSortUmElemento)
    , ("MergeSort Dois Elementos", testeMergeSortDoisElementos)
    , ("MergeSort Tr^es Elementos", testeMergeSortTresElementos)
    , ("MergeSort Ordenado", testeMergeSortOrdenado)
    , ("MergeSort Ordenado Par", testeMergeSortOrdenadoPar)
    , ("MergeSort Ordenado ´Impar", testeMergeSortOrdenadoImpar)
    , ("MergeSort Inverso", testeMergeSortInverso)
    , ("MergeSort Inverso Par", testeMergeSortInversoPar)
    , ("MergeSort Duplicados", testeMergeSortDuplicados)
    , ("MergeSort Todos Duplicados", testeMergeSortTodosDuplicados)
    , ("MergeSort Duplicatas Complexas", testeMergeSortDuplicatasComplexas)
    , ("MergeSort Negativos", testeMergeSortNegativos)
    , ("MergeSort Misto Sinais", testeMergeSortMistoSignos)
    , ("MergeSort Strings", testeMergeSortStrings)
    , ("MergeSort Caracteres", testeMergeSortCaracteres)
    , ("MergeSort Decimais", testeMergeSortDecimais)
    , ("MergeSort Tamanho 4", testeMergeSortTamanho4)
    , ("MergeSort Tamanho 8", testeMergeSortTamanho8)
    , ("MergeSort Tamanho 16", testeMergeSortTamanho16)
    , ("MergeSort Grande", testeMergeSortGrande)
    , ("MergeSort Estabilidade", testeMergeSortEstabilidade)

    -- InsertionSort (23 testes)
    , ("InsertionSort Vazio", testeInsertionSortVazio)
    , ("InsertionSort Um Elemento", testeInsertionSortUmElemento)
    , ("InsertionSort Dois Elementos", testeInsertionSortDoisElementos)
    , ("InsertionSort Dois Ordenados", testeInsertionSortDoisElementosOrdenados)
    , ("InsertionSort Tr^es Elementos", testeInsertionSortTresElementos)
    , ("InsertionSort Quatro Elementos", testeInsertionSortQuatroElementos)
    , ("InsertionSort Cinco Elementos", testeInsertionSortCincoElementos)
    , ("InsertionSort Ordenado", testeInsertionSortOrdenado)
    , ("InsertionSort Ordenado M´edio", testeInsertionSortOrdenadoMedio)
    , ("InsertionSort Inverso", testeInsertionSortInverso)
    , ("InsertionSort Inverso Grande", testeInsertionSortInversoGrande)
    , ("InsertionSort Duplicados", testeInsertionSortDuplicados)
    , ("InsertionSort Todos Duplicados", testeInsertionSortTodosDuplicados)
    , ("InsertionSort Duplicatas Parciais", testeInsertionSortDuplicatasParciais)
    , ("InsertionSort Negativos", testeInsertionSortNegativos)
    , ("InsertionSort Misto +/-", testeInsertionSortMistoPositivoNegativo)
    , ("InsertionSort Com Zero", testeInsertionSortComZero)
    , ("InsertionSort Strings", testeInsertionSortStrings)
    , ("InsertionSort Caracteres", testeInsertionSortCaracteres)
    , ("InsertionSort Decimais", testeInsertionSortDecimais)
    , ("InsertionSort Melhor Caso", testeInsertionSortMelhorCaso)
    , ("InsertionSort Pior Caso", testeInsertionSortPiorCaso)
    , ("InsertionSort Caso M´edio", testeInsertionSortCasoMedio)

    -- Testes de Consistência (11 testes)
    , ("Consistência 1", testeConsistencia1)
    , ("Consistência 2", testeConsistencia2)
    , ("Consistência Vazio", testeConsistenciaVazio)
    , ("Consistência Um Elemento", testeConsistenciaUmElemento)
    , ("Consistência Duplicados", testeConsistenciaDuplicados)
    , ("Consistência Negativos", testeConsistenciaNegativos)
    , ("Consistência Strings", testeConsistenciaStrings)
    , ("Consistência Decimais", testeConsistenciaDecimais)
    , ("Consistência Grande", testeConsistenciaGrande)
    , ("Consistência Caracteres", testeConsistenciaCaracteres)
    , ("Consistência Extremos", testeConsistenciaValoresExtremos)

    -- Testes de Propriedades (5 testes)
    , ("Propriedade Comprimento", testePropriedadeComprimento)
    , ("Propriedade Elementos", testePropriedadeElementos)
    , ("Propriedade Ordem", testePropriedadeOrdem)
    , ("Propriedade idempotência", testePropriedadeIdempotencia)
    , ("Propriedade Lista Vazia", testePropriedadeListaVazia)
    ]
