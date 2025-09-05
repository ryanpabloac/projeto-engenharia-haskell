module Tests.AlgorithmsTests where

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
-- TESTES DE CONSIST^ENCIA ENTRE ALGORITMOS
-- =================================

testeConsistenciaAlgoritmos :: Bool
testeConsistenciaAlgoritmos =
    let lista = [7, 2, 9, 1, 5, 3, 8, 4, 6]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
    in quick == merge && merge == insertion

testeConsistenciaVazio :: Bool
testeConsistenciaVazio =
    let lista = []
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
    in quick == merge && merge == insertion && quick == ([] :: [Int])

testeConsistenciaUmElemento :: Bool
testeConsistenciaUmElemento =
    let lista = [42]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
    in quick == merge && merge == insertion && quick == lista

testeConsistenciaNegativos :: Bool
testeConsistenciaNegativos =
    let lista = [-10, 5, -3, 8, -1]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
    in quick == merge && merge == insert

testeConsistenciaDuplicados :: Bool
testeConsistenciaDuplicados =
    let lista = [3, 1, 1, 3, 2, 2]
        quick = quickSort lista
        merge = mergeSort lista
        insertion = insertionSort lista
        esperado = [1, 1, 2, 2, 3, 3]
    in quick == esperado && merge == esperado && insertion == esperado

-- =================================
-- TESTES PARA BUSCAR PROJETO
-- =================================

testeBuscarProjetoExistente :: Bool
testeBuscarProjetoExistente =
    let projeto1 = Projeto 1 "Projeto A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projeto2 = Projeto 2 "Projeto B" Mecanica EmDesenvolvimento [] [] [] [] []
                            (fromGregorian 2025 2 1) Nothing 75000
        projetos = [projeto1, projeto2]
        resultado = buscarProjeto 1 projetos
    in case resultado of
        Just p -> idProjeto p == 1 && nomeProjeto p == "Projeto A"
        Nothing -> False

testeBuscarProjetoInexistente :: Bool
testeBuscarProjetoInexistente =
    let projeto1 = Projeto 1 "Projeto A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projetos = [projeto1]
        resultado = buscarProjeto 99 projetos
    in resultado == Nothing

testeBuscarProjetoListaVazia :: Bool
testeBuscarProjetoListaVazia =
    let projetos = []
        resultado = buscarProjeto 1 projetos
    in resultado == Nothing

testeBuscarProjetoPrimeiro :: Bool
testeBuscarProjetoPrimeiro =
    let projetos = [Projeto i ("Projeto " ++ show i) Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 i) Nothing (fromIntegral i * 1000)
                    | i <- [1..10]]
        resultado = buscarProjeto 1 projetos
    in case resultado of
        Just p -> idProjeto p == 1 && nomeProjeto p == "Projeto 1"
        Nothing -> False

testeBuscarProjetoUltimo :: Bool
testeBuscarProjetoUltimo =
    let projetos = [Projeto i ("Projeto " ++ show i) Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing (fromIntegral i * 1000)
                    | i <- [1..10]]
        resultado = buscarProjeto 10 projetos
    in case resultado of
        Just p -> idProjeto p == 10 && nomeProjeto p == "Projeto 10"
        Nothing -> False

testeBuscarProjetoMeio :: Bool
testeBuscarProjetoMeio =
    let projetos = [Projeto i ("Projeto " ++ show i) Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing (fromIntegral i * 1000)
                    | i <- [1..10]]
        resultado = buscarProjeto 5 projetos
    in case resultado of
        Just p -> idProjeto p == 5
        Nothing -> False

testeBuscarProjetoTipoEspecifico :: Bool
testeBuscarProjetoTipoEspecifico =
    let projeto1 = Projeto 1 "Civil A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projeto2 = Projeto 2 "Mecanica B" Mecanica EmDesenvolvimento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 60000
        projetos = [projeto1, projeto2]
        resultado = buscarProjeto 2 projetos
    in case resultado of
        Just p -> tipoProjeto p == Mecanica
        Nothing -> False

testeBuscarProjetoStatusEspecifico :: Bool
testeBuscarProjetoStatusEspecifico =
    let projeto = Projeto 100 "Projeto Especial" Eletrica Concluido [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 6 1)) 100000
        projetos = [projeto]
        resultado = buscarProjeto 100 projetos
    in case resultado of
        Just p -> status p == Concluido
        Nothing -> False

testeBuscarProjetoOrcamento :: Bool
testeBuscarProjetoOrcamento =
    let projeto = Projeto 50 "Projeto Caro" Estrutural EmRevisao [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 999999
        projetos = [projeto]
        resultado = buscarProjeto 50 projetos
    in case resultado of
        Just p -> orcamento p == 999999
        Nothing -> False

testeBuscarProjetoComDuplicataId :: Bool
testeBuscarProjetoComDuplicataId =
-- Assumindo que IDs ´unicos s~ao garantidos, mas testando comportamento
    let projeto1 = Projeto 5 "Primeiro" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 10000
        projeto2 = Projeto 5 "Segundo" Mecanica EmDesenvolvimento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 20000
        projetos = [projeto1, projeto2]
        resultado = buscarProjeto 5 projetos
    in case resultado of
        Just p -> nomeProjeto p == "Primeiro" -- deve retornar o primeiro encontrado
        Nothing -> False

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
-- TESTES PARA CONSTRUIR ´ARVORE
-- =================================

testeConstruirArvoreVazia :: Bool
testeConstruirArvoreVazia =
    construirArvore [] == (Vazia :: ArvoreBinaria Int)

testeConstruirArvoreUmElemento :: Bool
testeConstruirArvoreUmElemento =
    construirArvore [5] == No 5 Vazia Vazia

testeConstruirArvoreOrdenada :: Bool
testeConstruirArvoreOrdenada =
    let lista = [1, 2, 3, 4, 5]
        arvore = construirArvore lista
    in arvore /= Vazia

testeConstruirArvoreDesordenada :: Bool
testeConstruirArvoreDesordenada =
    let lista = [3, 1, 4, 1, 5, 9, 2, 6]
        arvore = construirArvore lista
    in arvore /= Vazia

testeConstruirArvoreStrings :: Bool
testeConstruirArvoreStrings =
    let lista = ["banana", "apple", "cherry"]
        arvore = construirArvore lista
    in arvore /= Vazia

testeConstruirArvoreNegativos :: Bool
testeConstruirArvoreNegativos =
    let lista = [-5, -2, -8, -1, -10]
        arvore = construirArvore lista
    in arvore /= Vazia

testeConstruirArvoreDuplicados :: Bool
testeConstruirArvoreDuplicados =
    let lista = [5, 3, 5, 1, 3, 7]
        arvore = construirArvore lista
    in arvore /= Vazia

testeConstruirArvoreDecimal :: Bool
testeConstruirArvoreDecimal =
    let lista = [3.14, 2.71, 1.41, 1.73]
        arvore = construirArvore lista
    in arvore /= Vazia

testeConstruirArvoreGrande :: Bool
testeConstruirArvoreGrande =
    let lista = [1..20]
        arvore = construirArvore lista
    in arvore /= Vazia

testeConstruirArvoreCaracteres :: Bool
testeConstruirArvoreCaracteres =
    let lista = ['m', 'a', 't', 'e', 'm', 'a', 't', 'i', 'c', 'a']
        arvore = construirArvore lista
    in arvore /= Vazia

-- =================================
-- TESTES PARA BUSCAR ´ARVORE
-- =================================

testeBuscarArvoreElementoExiste :: Bool
testeBuscarArvoreElementoExiste =
    let arvore = construirArvore [5, 3, 7, 1, 9]
    in buscarArvore 7 arvore

testeBuscarArvoreElementoNaoExiste :: Bool
testeBuscarArvoreElementoNaoExiste =
    let arvore = construirArvore [5, 3, 7, 1, 9]
    in not (buscarArvore 4 arvore)

testeBuscarArvoreVazia :: Bool
testeBuscarArvoreVazia =
    not (buscarArvore 5 (Vazia :: ArvoreBinaria Int))

testeBuscarArvoreRaiz :: Bool
testeBuscarArvoreRaiz =
    let arvore = construirArvore [10, 5, 15, 3, 7, 12, 18]
    in buscarArvore 10 arvore

testeBuscarArvoreFolha :: Bool
testeBuscarArvoreFolha =
    let arvore = construirArvore [10, 5, 15]
    in buscarArvore 5 arvore && buscarArvore 15 arvore

testeBuscarArvoreNegativos :: Bool
testeBuscarArvoreNegativos =
    let arvore = construirArvore [-10, -5, -15, -3, -7]
    in buscarArvore (-5) arvore && not (buscarArvore 0 arvore)

testeBuscarArvoreStrings :: Bool
testeBuscarArvoreStrings =
    let arvore = construirArvore ["banana", "apple", "cherry", "date"]
    in buscarArvore "apple" arvore && not (buscarArvore "zebra" arvore)

testeBuscarArvoreDuplicados :: Bool
testeBuscarArvoreDuplicados =
    let arvore = construirArvore [5, 3, 5, 1, 3, 7]
    in buscarArvore 5 arvore && buscarArvore 3 arvore

testeBuscarArvoreUmElemento :: Bool
testeBuscarArvoreUmElemento =
    let arvore = construirArvore [42]
    in buscarArvore 42 arvore && not (buscarArvore 41 arvore)

testeBuscarArvoreCompleta :: Bool
testeBuscarArvoreCompleta =
    let lista = [1..15]
        arvore = construirArvore lista
        todosPresentse = all (\x -> buscarArvore x arvore) lista
        nenhumAusente = not (any (\x -> buscarArvore x arvore) [16..20])
    in todosPresentes && nenhumAusente

-- =================================
-- TESTES PARA FILTRAR PROJETOS
-- =================================

testeFiltrarProjetosTipo :: Bool
testeFiltrarProjetosTipo =
    let projeto1 = Projeto 1 "A" Civil Concluido [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projeto2 = Projeto 2 "B" Civil EmDesenvolvimento [] [] [] [] []
                            (fromGregorian 2025 2 1) Nothing 60000
        projeto3 = Projeto 3 "C" Mecanica Concluido [] [] [] [] []
                            (fromGregorian 2025 3 1) Nothing 70000
        projetos = [projeto1, projeto2, projeto3]
        filtroTipo = filtrarProjetos (\p -> tipoProjeto p == Civil) projetos
    in length filtroTipo == 2

testeFiltrarProjetosStatus :: Bool
testeFiltrarProjetosStatus =
    let projeto1 = Projeto 1 "A" Civil Concluido [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projeto2 = Projeto 2 "B" Civil EmDesenvolvimento [] [] [] [] []
                            (fromGregorian 2025 2 1) Nothing 60000
        projeto3 = Projeto 3 "C" Mecanica Concluido [] [] [] [] []
                            (fromGregorian 2025 3 1) Nothing 70000
        projetos = [projeto1, projeto2, projeto3]
        filtroStatus = filtrarProjetos (\p -> status p == Concluido) projetos
    in length filtroStatus == 2

testeFiltrarProjetosOrcamento :: Bool
testeFiltrarProjetosOrcamento =
    let projetos = [Projeto i ("Projeto " ++ show i) Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing (fromIntegral i * 10000)
                    | i <- [1..10]]
        filtroOrcamento = filtrarProjetos (\p -> orcamento p > 50000) projetos
    in length filtroOrcamento == 5 -- projetos 6,7,8,9,10

testeFiltrarProjetosVazio :: Bool
testeFiltrarProjetosVazio =
    let projetos = []
        resultado = filtrarProjetos (\p -> tipoProjeto p == Civil) projetos
    in null resultado

testeFiltrarProjetosNenhumAtende :: Bool
testeFiltrarProjetosNenhumAtende =
    let projetos = [Projeto i ("Projeto " ++ show i) Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 10000
                    | i <- [1..5]]
        resultado = filtrarProjetos (\p -> orcamento p > 100000) projetos
    in null resultado

testeFiltrarProjetosTodosAtendem :: Bool
testeFiltrarProjetosTodosAtendem =
    let projetos = [Projeto i ("Projeto " ++ show i) Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 10000
                    | i <- [1..5]]
        resultado = filtrarProjetos (\p -> orcamento p > 0) projetos
    in length resultado == 5

testeFiltrarProjetosNome :: Bool
testeFiltrarProjetosNome =
    let projeto1 = Projeto 1 "Projeto Especial" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projeto2 = Projeto 2 "Outro Projeto" Mecanica EmDesenvolvimento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 60000
        projeto3 = Projeto 3 "Projeto Normal" Civil Concluido [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 70000
        projetos = [projeto1, projeto2, projeto3]
        resultado = filtrarProjetos (\p -> "Projeto" elem words (nomeProjeto p)) projetos
    in length resultado == 3 -- todos cont^em "Projeto"

testeFiltrarProjetosComposicao :: Bool
testeFiltrarProjetosComposicao =
    let projetos = [Projeto i ("Projeto " ++ show i) tipo status [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing (fromIntegral i * 10000)
                    | i <- [1..10]
                    , let tipo = if even i then Civil else Mecanica
                    , let status = if i <= 5 then Planejamento else Concluido]
        -- Filtrar projetos Civil E Conclu´ıdo E or¸camento > 60000
        resultado = filtrarProjetos (\p -> tipoProjeto p == Civil &&
                                        status p == Concluido &&
                                        orcamento p > 60000) projetos
    in length resultado == 2 -- projetos 8 e 10

testeFiltrarProjetosData :: Bool
testeFiltrarProjetosData =
    let projeto1 = Projeto 1 "A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 15) Nothing 50000
        projeto2 = Projeto 2 "B" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 2 15) Nothing 60000
        projeto3 = Projeto 3 "C" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 3 15) Nothing 70000
        projetos = [projeto1, projeto2, projeto3]
        dataLimite = fromGregorian 2025 2 1
        resultado = filtrarProjetos (\p -> dataInicio p >= dataLimite) projetos
    in length resultado == 2 -- projetos 2 e 3

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

    putStrLn "\n-- TESTES CONSIST^ENCIA --"
    putStrLn $ "Algoritmos: " ++ show testeConsistenciaAlgoritmos
    putStrLn $ "Vazio: " ++ show testeConsistenciaVazio
    putStrLn $ "Um elemento: " ++ show testeConsistenciaUmElemento
    putStrLn $ "Negativos: " ++ show testeConsistenciaNegativos
    putStrLn $ "Duplicados: " ++ show testeConsistenciaDuplicados

    putStrLn "\n-- TESTES BUSCAR PROJETO --"

    putStrLn $ "Existente: " ++ show testeBuscarProjetoExistente
    putStrLn $ "Inexistente: " ++ show testeBuscarProjetoInexistente
    putStrLn $ "Lista vazia: " ++ show testeBuscarProjetoListaVazia
    putStrLn $ "Primeiro: " ++ show testeBuscarProjetoPrimeiro
    putStrLn $ "´Ultimo: " ++ show testeBuscarProjetoUltimo
    putStrLn $ "Meio: " ++ show testeBuscarProjetoMeio
    putStrLn $ "Tipo espec´ıfico: " ++ show testeBuscarProjetoTipoEspecifico
    putStrLn $ "Status espec´ıfico: " ++ show testeBuscarProjetoStatusEspecifico
    putStrLn $ "Or¸camento: " ++ show testeBuscarProjetoOrcamento
    putStrLn $ "ID duplicado: " ++ show testeBuscarProjetoComDuplicataId

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

    putStrLn "\n-- TESTES CONSTRUIR ´ARVORE --"
    putStrLn $ "Vazia: " ++ show testeConstruirArvoreVazia
    putStrLn $ "Um elemento: " ++ show testeConstruirArvoreUmElemento
    putStrLn $ "Ordenada: " ++ show testeConstruirArvoreOrdenada
    putStrLn $ "Desordenada: " ++ show testeConstruirArvoreDesordenada
    putStrLn $ "Strings: " ++ show testeConstruirArvoreStrings
    putStrLn $ "Negativos: " ++ show testeConstruirArvoreNegativos
    putStrLn $ "Duplicados: " ++ show testeConstruirArvoreDuplicados
    putStrLn $ "Decimal: " ++ show testeConstruirArvoreDecimal
    putStrLn $ "Grande: " ++ show testeConstruirArvoreGrande
    putStrLn $ "Caracteres: " ++ show testeConstruirArvoreCaracteres

    putStrLn "\n-- TESTES BUSCAR ´ARVORE --"
    putStrLn $ "Elemento existe: " ++ show testeBuscarArvoreElementoExiste
    putStrLn $ "Elemento n~ao existe: " ++ show testeBuscarArvoreElementoNaoExiste
    putStrLn $ "Vazia: " ++ show testeBuscarArvoreVazia
    putStrLn $ "Raiz: " ++ show testeBuscarArvoreRaiz
    putStrLn $ "Folha: " ++ show testeBuscarArvoreFolha
    putStrLn $ "Negativos: " ++ show testeBuscarArvoreNegativos
    putStrLn $ "Strings: " ++ show testeBuscarArvoreStrings
    putStrLn $ "Duplicados: " ++ show testeBuscarArvoreDuplicados
    putStrLn $ "Um elemento: " ++ show testeBuscarArvoreUmElemento
    putStrLn $ "Completa: " ++ show testeBuscarArvoreCompleta

    putStrLn "\n-- TESTES FILTRAR PROJETOS --"
    putStrLn $ "Tipo: " ++ show testeFiltrarProjetosTipo
    putStrLn $ "Status: " ++ show testeFiltrarProjetosStatus
    putStrLn $ "Or¸camento: " ++ show testeFiltrarProjetosOrcamento
    putStrLn $ "Vazio: " ++ show testeFiltrarProjetosVazio
    putStrLn $ "Nenhum atende: " ++ show testeFiltrarProjetosNenhumAtende
    putStrLn $ "Todos atendem: " ++ show testeFiltrarProjetosTodosAtendem
    putStrLn $ "Nome: " ++ show testeFiltrarProjetosNome
    putStrLn $ "Composi¸c~ao: " ++ show testeFiltrarProjetosComposicao
    putStrLn $ "Data: " ++ show testeFiltrarProjetosData

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

        -- Consist^encia
        , ("Consist^encia Algoritmos", testeConsistenciaAlgoritmos)
        , ("Consist^encia Vazio", testeConsistenciaVazio)
        , ("Consist^encia Um Elemento", testeConsistenciaUmElemento)
        , ("Consist^encia Negativos", testeConsistenciaNegativos)
        , ("Consist^encia Duplicados", testeConsistenciaDuplicados)

        -- Buscar Projeto
        , ("Buscar Projeto Existente", testeBuscarProjetoExistente)
        , ("Buscar Projeto Inexistente", testeBuscarProjetoInexistente)
        , ("Buscar Projeto Lista Vazia", testeBuscarProjetoListaVazia)
        , ("Buscar Projeto Primeiro", testeBuscarProjetoPrimeiro)
        , ("Buscar Projeto ´Ultimo", testeBuscarProjetoUltimo)

        -- Inserir Ordenado
        , ("Inserir Ordenado Meio", testeInserirOrdenadoMeio)
        , ("Inserir Ordenado In´ıcio", testeInserirOrdenadoInicio)
        , ("Inserir Ordenado Fim", testeInserirOrdenadoFim)
        , ("Inserir Ordenado Vazio", testeInserirOrdenadoVazio)
        , ("Inserir Ordenado Duplicado", testeInserirOrdenadoDuplicado)

        -- Construir ´Arvore
        , ("Construir ´Arvore Vazia", testeConstruirArvoreVazia)
        , ("Construir ´Arvore Um Elemento", testeConstruirArvoreUmElemento)
        , ("Construir ´Arvore Ordenada", testeConstruirArvoreOrdenada)
        , ("Construir ´Arvore Desordenada", testeConstruirArvoreDesordenada)
        , ("Construir ´Arvore Strings", testeConstruirArvoreStrings)

        -- Buscar ´Arvore
        , ("Buscar ´Arvore Elemento Existe", testeBuscarArvoreElementoExiste)
        , ("Buscar ´Arvore Elemento N~ao Existe", testeBuscarArvoreElementoNaoExiste)
        , ("Buscar ´Arvore Vazia", testeBuscarArvoreVazia)
        , ("Buscar ´Arvore Raiz", testeBuscarArvoreRaiz)
        , ("Buscar ´Arvore Folha", testeBuscarArvoreFolha)

        -- Filtrar Projetos
        , ("Filtrar Projetos Tipo", testeFiltrarProjetosTipo)
        , ("Filtrar Projetos Status", testeFiltrarProjetosStatus)
        , ("Filtrar Projetos Or¸camento", testeFiltrarProjetosOrcamento)
        , ("Filtrar Projetos Vazio", testeFiltrarProjetosVazio)
        , ("Filtrar Projetos Nenhum Atende", testeFiltrarProjetosNenhumAtende)
    ]
