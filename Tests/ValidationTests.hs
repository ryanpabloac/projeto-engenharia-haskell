module Tests.ValidationTests where

import Types
import Validation
import Reports
import Interface
import Data.Time.Calendar (fromGregorian, Day)
import qualified Data.Map as Map

-- =================================
-- TESTES PARA VALIDAR PROJETO
-- =================================

testeValidarProjetoValido :: Bool
testeValidarProjetoValido =
    let projeto = Projeto 1 "Projeto V´alido" Civil Planejamento [] []
                            [Material "Concreto" 2400 30e6 350 100] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        erros = validarProjeto projeto
    in null erros

testeValidarProjetoNomeVazio :: Bool
testeValidarProjetoNomeVazio =
    let projeto = Projeto 1 "" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        erros = validarProjeto projeto
    in not (null erros) && any (isInfixOf "nome") (map toLower erros)
        where
            toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)
            isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
            isPrefixOf [] _ = True
            isPrefixOf _ [] = False
            isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
            tails [] = [[]]
            tails xs@(_:ys) = xs : tails ys

testeValidarProjetoOrcamentoNegativo :: Bool
testeValidarProjetoOrcamentoNegativo =
    let projeto = Projeto 1 "Projeto" Civil Planejamento [] [] [] [] []
                        (fromGregorian 2025 1 1) Nothing (-1000)
        erros = validarProjeto projeto
    in not (null erros) && length erros >= 1

testeValidarProjetoSemMateriais :: Bool
testeValidarProjetoSemMateriais =
    let projeto = Projeto 1 "Projeto Sem Materiais" Civil EmDesenvolvimento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        erros = validarProjeto projeto
    in not (null erros)

testeValidarProjetoDataInvalidaFim :: Bool
testeValidarProjetoDataInvalidaFim =
    let projeto = Projeto 1 "Projeto" Civil Planejamento [] []
                            [Material "Concreto" 2400 30e6 350 100] [] []
                            (fromGregorian 2025 6 1) (Just (fromGregorian 2025 1 1)) 50000
        erros = validarProjeto projeto
    in not (null erros) -- data fim anterior `a data in´ıcio

testeValidarProjetoIdNegativo :: Bool
testeValidarProjetoIdNegativo =
    let projeto = Projeto (-1) "Projeto" Civil Planejamento [] []
                            [Material "Concreto" 2400 30e6 350 100] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        erros = validarProjeto projeto
    in not (null erros)

testeValidarProjetoCompleto :: Bool
testeValidarProjetoCompleto =
    let material1 = Material "Concreto" 2400 30e6 350 100
        material2 = Material "A¸co" 7850 500e6 4.50 50
        calculo1 = CalculoMat "Momento In´ercia" [0.3, 0.5] 0.003125 "m^4" "I=bh³/12"
        funcao1 = Funcao (Linear 2 3) "f(x) = 2x + 3"
        projeto = Projeto 1 "Projeto Completo" Civil EmDesenvolvimento [] []
                            [material1, material2] [calculo1] [funcao1]
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 12 31)) 100000
        erros = validarProjeto projeto
    in null erros

testeValidarProjetoMaterialInvalido :: Bool
testeValidarProjetoMaterialInvalido =
    let materialInvalido = Material "Material Inv´alido" (-100) (-50) (-10) 0
        projeto = Projeto 1 "Projeto" Civil Planejamento [] []
                            [materialInvalido] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        erros = validarProjeto projeto
    in not (null erros) && length erros >= 3 -- densidade, resist^encia, custo negativos

testeValidarProjetoOrcamentoZero :: Bool
testeValidarProjetoOrcamentoZero =
    let projeto = Projeto 1 "Projeto Zero" Civil Planejamento [] []
                            [Material "Concreto" 2400 30e6 350 100] [] []
                            (fromGregorian 2025 1 1) Nothing 0
        erros = validarProjeto projeto
    in not (null erros)

testeValidarProjetoNomeMuitoLongo :: Bool
testeValidarProjetoNomeMuitoLongo =
    let nomeGigante = replicate 300 'A'
        projeto = Projeto 1 nomeGigante Civil Planejamento [] []
                            [Material "Concreto" 2400 30e6 350 100] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        erros = validarProjeto projeto
    in not (null erros)

-- =================================
-- TESTES PARA CALCULAR CUSTO TOTAL
-- =================================

testeCalcularCustoTotalBasico :: Bool
testeCalcularCustoTotalBasico =
    let material1 = Material "Concreto" 2400 30e6 350 100 -- 350 * 100 = 35000
        material2 = Material "A¸co" 7850 500e6 4.50 50 -- 4.50 * 50 = 225
        projeto = Projeto 1 "Projeto" Civil Planejamento [] []
                            [material1, material2] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        custoTotal = calcularCustoTotal projeto
        esperado = 35225.0 -- 35000 + 225
    in abs (custoTotal - esperado) < 0.01

testeCalcularCustoTotalSemMateriais :: Bool
testeCalcularCustoTotalSemMateriais =
    let projeto = Projeto 1 "Projeto Vazio" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        custoTotal = calcularCustoTotal projeto
    in custoTotal == 0.0

testeCalcularCustoTotalUmMaterial :: Bool
testeCalcularCustoTotalUmMaterial =
    let material = Material "Tijolo" 1800 15e6 0.50 2000 -- 0.50 * 2000 = 1000
        projeto = Projeto 1 "Casa de Tijolo" Civil Planejamento [] []
                            [material] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        custoTotal = calcularCustoTotal projeto
        esperado = 1000.0
    in abs (custoTotal - esperado) < 0.01

testeCalcularCustoTotalMaterialesCaros :: Bool
testeCalcularCustoTotalMaterialesCaros =
    let material1 = Material "Ouro" 19300 200e9 50000 0.001 -- 50000 * 0.001 = 50
        material2 = Material "Platina" 21450 170e9 60000 0.002 -- 60000 * 0.002 = 120
        projeto = Projeto 1 "Projeto Luxo" Civil Planejamento [] []
                            [material1, material2] [] []
                            (fromGregorian 2025 1 1) Nothing 1000000
        custoTotal = calcularCustoTotal projeto
        esperado = 170.0 -- 50 + 120
    in abs (custoTotal - esperado) < 0.01

testeCalcularCustoTotalQuantidadeZero :: Bool
testeCalcularCustoTotalQuantidadeZero =
    let material1 = Material "Concreto" 2400 30e6 350 0 -- 350 * 0 = 0
        material2 = Material "A¸co" 7850 500e6 4.50 100 -- 4.50 * 100 = 450
        projeto = Projeto 1 "Projeto" Civil Planejamento [] []
                            [material1, material2] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        custoTotal = calcularCustoTotal projeto
        esperado = 450.0
    in abs (custoTotal - esperado) < 0.01

testeCalcularCustoTotalDecimais :: Bool
testeCalcularCustoTotalDecimais =
    let material1 = Material "Material A" 1000 1e6 12.34 56.78 -- 12.34 * 56.78 = 700.5852
        material2 = Material "Material B" 2000 2e6 9.87 10.11 -- 9.87 * 10.11 = 99.7857
        projeto = Projeto 1 "Projeto Decimal" Civil Planejamento [] []
                            [material1, material2] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        custoTotal = calcularCustoTotal projeto
        esperado = 800.3709 -- 700.5852 + 99.7857
    in abs (custoTotal - esperado) < 0.01

testeCalcularCustoTotalMuitosMateriais :: Bool
testeCalcularCustoTotalMuitosMateriais =
    let materiais = [Material ("Material " ++ show i) 1000 1e6 10 5 | i <- [1..10]]
        -- Cada material: 10 * 5 = 50, total = 50 * 10 = 500
        projeto = Projeto 1 "Projeto Grande" Civil Planejamento [] []
                            materiais [] []
                            (fromGregorian 2025 1 1) Nothing 100000
        custoTotal = calcularCustoTotal projeto
        esperado = 500.0
    in abs (custoTotal - esperado) < 0.01

testeCalcularCustoTotalCustoZero :: Bool
testeCalcularCustoTotalCustoZero =
    let material1 = Material "Gratuito" 1000 1e6 0 1000 -- 0 * 1000 = 0
        material2 = Material "Pago" 2000 2e6 100 10 -- 100 * 10 = 1000
        projeto = Projeto 1 "Projeto Misto" Civil Planejamento [] []
                            [material1, material2] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        custoTotal = calcularCustoTotal projeto
        esperado = 1000.0
    in abs (custoTotal - esperado) < 0.01

testeCalcularCustoTotalValoresGrandes :: Bool
testeCalcularCustoTotalValoresGrandes =
    let material = Material "Caro" 1000 1e6 1000000 1000 -- 1M * 1000 = 1B
        projeto = Projeto 1 "Projeto Bilion´ario" Civil Planejamento [] []
                            [material] [] []
                            (fromGregorian 2025 1 1) Nothing 2000000000
        custoTotal = calcularCustoTotal projeto
        esperado = 1000000000.0 -- 1 bilh~ao
    in abs (custoTotal - esperado) < 1.0

testeCalcularCustoTotalPrecisao :: Bool
testeCalcularCustoTotalPrecisao =
    let material = Material "Precis~ao" 1000 1e6 0.001 0.001 -- 0.001 * 0.001 = 0.000001
        projeto = Projeto 1 "Projeto Micro" Civil Planejamento [] []
                            [material] [] []
                            (fromGregorian 2025 1 1) Nothing 1
        custoTotal = calcularCustoTotal projeto
        esperado = 0.000001
    in abs (custoTotal - esperado) < 0.0000001

-- =================================
-- TESTES PARA GERAR RELAT´ORIO PROJETO
-- =================================

testeGerarRelatorioProjetoCompleto :: Bool
testeGerarRelatorioProjetoCompleto =
    let material1 = Material "Concreto" 2400 30e6 350 100
        material2 = Material "A¸co" 7850 500e6 4.50 50
        calculo1 = CalculoMat "´Area" [10, 20] 200 "m²" "A = b * h"
        funcao1 = Funcao (Linear 2 3) "f(x) = 2x + 3"
        projeto = Projeto 1 "Ponte Rodovi´aria" Civil EmDesenvolvimento [] []
                            [material1, material2] [calculo1] [funcao1]
                            (fromGregorian 2025 3 1) (Just (fromGregorian 2025 12 1)) 100000
        relatorio = gerarRelatorioProjeto projeto
    in length relatorio > 10 &&
        any (isInfixOf "Ponte Rodovi´aria") relatorio &&
        any (isInfixOf "Civil") relatorio &&
        any (isInfixOf "Concreto") relatorio
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeGerarRelatorioProjetoMinimo :: Bool
testeGerarRelatorioProjetoMinimo =
    let projeto = Projeto 1 "Projeto M´ınimo" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 1000
        relatorio = gerarRelatorioProjeto projeto
    in length relatorio >= 5 &&
        any (isInfixOf "Projeto M´ınimo") relatorio
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeGerarRelatorioProjetoSemDataFim :: Bool
testeGerarRelatorioProjetoSemDataFim =
    let projeto = Projeto 1 "Projeto Indefinido" Mecanica EmRevisao [] []
                            [Material "Metal" 7000 200e6 5 100] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        relatorio = gerarRelatorioProjeto projeto
    in length relatorio >= 5 &&
        any (isInfixOf "Em andamento") relatorio ||
        any (isInfixOf "Indefinido") relatorio ||
        any (isInfixOf "N~ao definida") relatorio
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeGerarRelatorioProjetoConcluido :: Bool
testeGerarRelatorioProjetoConcluido =
    let projeto = Projeto 1 "Projeto Finalizado" Eletrica Concluido [] []
                            [Material "Cabo" 8900 100e9 2 1000] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 6 1)) 25000
        relatorio = gerarRelatorioProjeto projeto
    in length relatorio >= 5 &&
        any (isInfixOf "Concluido") relatorio ||
        any (isInfixOf "Finalizado") relatorio
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeGerarRelatorioProjetoMuitosMateriais :: Bool
testeGerarRelatorioProjetoMuitosMateriais =
    let materiais = [Material ("Mat" ++ show i) (1000 + fromIntegral i)
                            (1e6 * fromIntegral i) (fromIntegral i) 10 | i <- [1..5]]
        projeto = Projeto 1 "Projeto Rico em Materiais" Estrutural Planejamento [] []
                            materiais [] []
                            (fromGregorian 2025 1 1) Nothing 100000
        relatorio = gerarRelatorioProjeto projeto
    in length relatorio >= 10 &&
        length (filter (isInfixOf "Mat") relatorio) >= 5
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeGerarRelatorioProjetoComCalculos :: Bool
testeGerarRelatorioProjetoComCalculos =
    let calculos = [ CalculoMat "´Area Total" [100, 200] 20000 "m²" "A = l * w"
                    , CalculoMat "Volume" [20000, 0.15] 3000 "m³" "V = A * h"
                    , CalculoMat "Massa" [3000, 2400] 7200000 "kg" "m = V * §$\rho$§"
                    ]
        projeto = Projeto 1 "Projeto com C´alculos" Civil Planejamento [] []
                            [Material "Concreto" 2400 30e6 350 200] calculos []
                            (fromGregorian 2025 1 1) Nothing 500000
        relatorio = gerarRelatorioProjeto projeto
    in length relatorio >= 10 &&
        any (isInfixOf "´Area Total") relatorio &&
        any (isInfixOf "Volume") relatorio
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeGerarRelatorioProjetoOrcamentoAlto :: Bool
testeGerarRelatorioProjetoOrcamentoAlto =
    let projeto = Projeto 1 "Projeto Bilion´ario" Civil Planejamento [] []
                            [Material "Ouro" 19300 200e9 50000 20] [] []
                            (fromGregorian 2025 1 1) Nothing 10000000
        relatorio = gerarRelatorioProjeto projeto
    in length relatorio >= 5 &&
        any (isInfixOf "10000000") relatorio ||
        any (isInfixOf "10.000.000") relatorio ||
        any (isInfixOf "bilion") relatorio
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeGerarRelatorioProjetoTiposVariados :: Bool
testeGerarRelatorioProjetoTiposVariados =
    let projetoCivil = Projeto 1 "Civil" Civil Planejamento [] [] [] [] []
                                (fromGregorian 2025 1 1) Nothing 1000
        projetoMecanica = Projeto 2 "Mec^anica" Mecanica EmDesenvolvimento [] [] [] [] []
                                (fromGregorian 2025 1 1) Nothing 2000
        projetoEletrica = Projeto 3 "El´etrica" Eletrica EmRevisao [] [] [] [] []
                                (fromGregorian 2025 1 1) Nothing 3000
        projetoEstrutural = Projeto 4 "Estrutural" Estrutural Concluido [] [] [] [] []
                                (fromGregorian 2025 1 1) Nothing 4000
        relatorios = map gerarRelatorioProjeto [projetoCivil, projetoMecanica, projetoEletrica, projetoEstrutural]
    in all (\r -> length r >= 3) relatorios

testeGerarRelatorioProjetoFormatacao :: Bool
testeGerarRelatorioProjetoFormatacao =
    let projeto = Projeto 1 "Teste Formata¸c~ao" Civil Planejamento [] []
                            [Material "Teste" 1000 1e6 100 10] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        relatorio = gerarRelatorioProjeto projeto
        temLinhasSeparadoras = any (all (\c -> c == '=' || c == '-')) relatorio
        temSecoes = any (isInfixOf "RELAT") relatorio ||
                    any (isInfixOf "PROJETO") relatorio
    in length relatorio >= 5 && (temLinhasSeparadoras || temSecoes)
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeGerarRelatorioProjetoComFuncoes :: Bool
testeGerarRelatorioProjetoComFuncoes =
    let funcoes = [ Funcao (Linear 2 3) "Fun¸c~ao linear: f(x) = 2x + 3"
                    , Funcao (Quadratica 1 (-2) 1) "Par´abola: f(x) = x² - 2x + 1"
                    , Funcao (Trigonometrica Seno 1 0) "Senoide: f(x) = sin(x)"
                    ]
        projeto = Projeto 1 "Projeto Matem´atico" Civil Planejamento [] [] [] [] funcoes
                            (fromGregorian 2025 1 1) Nothing 75000
        relatorio = gerarRelatorioProjeto projeto
    in length relatorio >= 8 &&
        any (isInfixOf "linear") relatorio ||
        any (isInfixOf "function") relatorio ||
        any (isInfixOf "matemat") relatorio
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

-- =================================
-- TESTES PARA COMPARAR PROJETOS
-- =================================

testeCompararProjetosIguais :: Bool
testeCompararProjetosIguais =
    let projeto = Projeto 1 "Projeto A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        resultado = compararProjetos projeto projeto
    in isInfixOf "iguais" resultado || isInfixOf "mesmo" resultado ||
        isInfixOf "identic" resultado
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeCompararProjetosDiferentesTipo :: Bool
testeCompararProjetosDiferentesTipo =
    let projeto1 = Projeto 1 "Projeto Civil" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projeto2 = Projeto 2 "Projeto Mec^anico" Mecanica EmDesenvolvimento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 60000
        resultado = compararProjetos projeto1 projeto2
    in isInfixOf "Civil" resultado && isInfixOf "Mecanica" resultado &&
        length resultado > 50
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        iPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeCompararProjetosOrcamentoDiferente :: Bool
testeCompararProjetosOrcamentoDiferente =
    let projeto1 = Projeto 1 "Barato" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 10000
        projeto2 = Projeto 2 "Caro" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 100000
        resultado = compararProjetos projeto1 projeto2
    in (isInfixOf "10000" resultado || isInfixOf "10.000" resultado) &&
        (isInfixOf "100000" resultado || isInfixOf "100.000" resultado) &&
        (isInfixOf "maior" resultado || isInfixOf "menor" resultado)
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeCompararProjetosStatusDiferente :: Bool
testeCompararProjetosStatusDiferente =
    let projeto1 = Projeto 1 "Em Planejamento" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projeto2 = Projeto 2 "Conclu´ıdo" Civil Concluido [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 6 1)) 50000
        resultado = compararProjetos projeto1 projeto2
    in isInfixOf "Planejamento" resultado &&
        (isInfixOf "Concluido" resultado || isInfixOf "finaliz" resultado)
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeCompararProjetosDatasDiferentes :: Bool
testeCompararProjetosDatasDiferentes =
    let projeto1 = Projeto 1 "Antigo" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projeto2 = Projeto 2 "Recente" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 6 1) Nothing 50000
        resultado = compararProjetos projeto1 projeto2
    in isInfixOf "2025" resultado && length resultado > 30
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeCompararProjetosMateriais :: Bool
testeCompararProjetosMateriais =
    let projeto1 = Projeto 1 "Com Material" Civil Planejamento [] []
                            [Material "Concreto" 2400 30e6 350 100] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projeto2 = Projeto 2 "Sem Material" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        resultado = compararProjetos projeto1 projeto2
    in isInfixOf "material" resultado &&
        (isInfixOf "1" resultado && isInfixOf "0" resultado)
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeCompararProjetosComplexidade :: Bool
testeCompararProjetosComplexidade =
    let material1 = Material "Concreto" 2400 30e6 350 100
        material2 = Material "A¸co" 7850 500e6 4.50 50
        calculo1 = CalculoMat "´Area" [10, 20] 200 "m²" "A = b * h"
        funcao1 = Funcao (Linear 2 3) "f(x) = 2x + 3"
        projetoSimples = Projeto 1 "Simples" Civil Planejamento [] [] [] [] []
                                (fromGregorian 2025 1 1) Nothing 10000
        projetoComplexo = Projeto 2 "Complexo" Civil Planejamento [] []
                                [material1, material2] [calculo1] [funcao1]
                                (fromGregorian 2025 1 1) Nothing 100000
        resultado = compararProjetos projetoSimples projetoComplexo
    in length resultado > 100 &&
        (isInfixOf "simples" resultado || isInfixOf "complex" resultado)
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeCompararProjetosNomesSimilares :: Bool
testeCompararProjetosNomesSimilares =
    let projeto1 = Projeto 1 "Ponte do Rio A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        projeto2 = Projeto 2 "Ponte do Rio B" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 55000
        resultado = compararProjetos projeto1 projeto2
    in isInfixOf "Ponte" resultado && isInfixOf "Rio" resultado
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeCompararProjetosComDataFim :: Bool
testeCompararProjetosComDataFim =
    let projeto1 = Projeto 1 "Com Fim" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 12 1)) 50000
        projeto2 = Projeto 2 "Sem Fim" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 50000
        resultado = compararProjetos projeto1 projeto2
    in isInfixOf "fim" resultado || isInfixOf "conclus" resultado ||
        isInfixOf "definid" resultado
    where
        isInfixOf xs ys = any (isPrefixOf xs) (tails ys)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:ys) = xs : tails ys

testeCompararProjetosFormatacao :: Bool
testeCompararProjetosFormatacao =
    let projeto1 = Projeto 1 "A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 1000
        projeto2 = Projeto 2 "B" Mecanica Concluido [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 2000
        resultado = compararProjetos projeto1 projeto2
    in length resultado > 20 &&
        (any (== ':') resultado || any (== '=') resultado || any (== '-') resultado)

-- =================================
-- TESTES PARA ESTAT´ISTICAS B´ASICAS
-- =================================

testeEstatisticasBasicasNormal :: Bool
testeEstatisticasBasicasNormal =
    let valores = [1.0, 2.0, 3.0, 4.0, 5.0]
        (media, maximo, minimo) = estatisticasBasicas valores
    in abs (media - 3.0) < 0.001 &&
        abs (maximo - 5.0) < 0.001 &&
        abs (minimo - 1.0) < 0.001

testeEstatisticasBasicasUmElemento :: Bool
testeEstatisticasBasicasUmElemento =
    let valores = [42.0]
        (media, maximo, minimo) = estatisticasBasicas valores
    in abs (media - 42.0) < 0.001 &&
        abs (maximo - 42.0) < 0.001 &&
        abs (minimo - 42.0) < 0.001

testeEstatisticasBasicasNegativos :: Bool
testeEstatisticasBasicasNegativos =
    let valores = [-10.0, -5.0, 0.0, 5.0, 10.0]
        (media, maximo, minimo) = estatisticasBasicas valores
    in abs media < 0.001 && -- m´edia deve ser 0
        abs (maximo - 10.0) < 0.001 &&
        abs (minimo - (-10.0)) < 0.001

testeEstatisticasBasicasDecimais :: Bool
testeEstatisticasBasicasDecimais =
    let valores = [1.1, 2.2, 3.3, 4.4, 5.5]
        (media, maximo, minimo) = estatisticasBasicas valores
        mediaEsperada = (1.1 + 2.2 + 3.3 + 4.4 + 5.5) / 5
    in abs (media - mediaEsperada) < 0.001 &&
        abs (maximo - 5.5) < 0.001 &&
        abs (minimo - 1.1) < 0.001

testeEstatisticasBasicasDuplicados :: Bool
testeEstatisticasBasicasDuplicados =
    let valores = [7.0, 7.0, 7.0, 7.0, 7.0]
        (media, maximo, minimo) = estatisticasBasicas valores
    in abs (media - 7.0) < 0.001 &&
        abs (maximo - 7.0) < 0.001 &&
        abs (minimo - 7.0) < 0.001

testeEstatisticasBasicasOrdemAleatoria :: Bool
testeEstatisticasBasicasOrdemAleatoria =
    let valores = [5.0, 1.0, 9.0, 3.0, 7.0]
        (media, maximo, minimo) = estatisticasBasicas valores
        mediaEsperada = 25.0 / 5.0 -- 5.0
    in abs (media - mediaEsperada) < 0.001 &&
        abs (maximo - 9.0) < 0.001 &&
        abs (minimo - 1.0) < 0.001

testeEstatisticasBasicasGrandes :: Bool
testeEstatisticasBasicasGrandes =
    let valores = [1000000.0, 2000000.0, 3000000.0]
        (media, maximo, minimo) = estatisticasBasicas valores
        mediaEsperada = 2000000.0
    in abs (media - mediaEsperada) < 1.0 &&
        abs (maximo - 3000000.0) < 1.0 &&
        abs (minimo - 1000000.0) < 1.0

testeEstatisticasBasicasPequenos :: Bool
testeEstatisticasBasicasPequenos =
    let valores = [0.001, 0.002, 0.003, 0.004, 0.005]
        (media, maximo, minimo) = estatisticasBasicas valores
        mediaEsperada = 0.003
    in abs (media - mediaEsperada) < 0.0001 &&
        abs (maximo - 0.005) < 0.0001 &&
        abs (minimo - 0.001) < 0.0001

testeEstatisticasBasicasZeros :: Bool
testeEstatisticasBasicasZeros =
    let valores = [0.0, 0.0, 0.0]
        (media, maximo, minimo) = estatisticasBasicas valores
    in abs media < 0.001 &&
        abs maximo < 0.001 &&
        abs minimo < 0.001

testeEstatisticasBasicasMuitosValores :: Bool
testeEstatisticasBasicasMuitosValores =
    let valores = [fromIntegral i | i <- [1..100]] -- 1 a 100
        (media, maximo, minimo) = estatisticasBasicas valores
        mediaEsperada = 50.5 -- (1+100)/2
    in abs (media - mediaEsperada) < 0.1 &&
        abs (maximo - 100.0) < 0.001 &&
        abs (minimo - 1.0) < 0.001

-- =================================
-- TESTES PARA CONTAR POR TIPO
-- =================================

testeContarPorTipoUnico :: Bool
testeContarPorTipoUnico =
    let projetos = [Projeto i ("P" ++ show i) Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 1000 | i <- [1..5]]
        resultado = contarPorTipo projetos
        esperado = [(Civil, 5)]
    in resultado == esperado

testeContarPorTipoVario :: Bool
testeContarPorTipoVario =
    let projetos = [ Projeto 1 "A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 1000
                    , Projeto 2 "B" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 2000
                    , Projeto 3 "C" Mecanica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 3000
                    , Projeto 4 "D" Eletrica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 4000
                    ]
        resultado = contarPorTipo projetos
        civilCount = lookup Civil resultado
        mecanicaCount = lookup Mecanica resultado
        eletricaCount = lookup Eletrica resultado
    in civilCount == Just 2 && mecanicaCount == Just 1 && eletricaCount == Just 1

testeContarPorTipoVazio :: Bool
testeContarPorTipoVazio =
    let projetos = []
        resultado = contarPorTipo projetos
    in null resultado

testeContarPorTipoTodosIguais :: Bool
testeContarPorTipoTodosIguais =
    let projetos = replicate 10 (Projeto 1 "Teste" Estrutural Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 1000)
        resultado = contarPorTipo projetos
        esperado = [(Estrutural, 10)]
    in resultado == esperado

testeContarPorTipoCompleto :: Bool
testeContarPorTipoCompleto =
    let projetos = [ Projeto 1 "Civil1" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 1000
                    , Projeto 2 "Civil2" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 2000
                    , Projeto 3 "Mec1" Mecanica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 3000
                    , Projeto 4 "Mec2" Mecanica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 4000
                    , Projeto 5 "Mec3" Mecanica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 5000
                    , Projeto 6 "Elet1" Eletrica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 6000
                    , Projeto 7 "Estr1" Estrutural Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 7000
                    ]
        resultado = contarPorTipo projetos
        totalProjetos = sum (map snd resultado)
        temTodosOsTipos = length resultado == 4 -- Civil, Mecanica, Eletrica, Estrutural
    in totalProjetos == 7 && temTodosOsTipos

testeContarPorTipoOrdem :: Bool
testeContarPorTipoOrdem =
    let projetos = [ Projeto 1 "Z" Estrutural Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 1000
                    , Projeto 2 "A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 2000
                    , Projeto 3 "M" Mecanica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 3000
                    , Projeto 4 "E" Eletrica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 4000
                    ]
        resultado = contarPorTipo projetos
    in length resultado == 4 && all (\(_, count) -> count == 1) resultado

testeContarPorTipoUmDeCadaTipo :: Bool
testeContarPorTipoUmDeCadaTipo =
    let projetos = [ Projeto 1 "Civil" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 1000
                    , Projeto 2 "Mecanica" Mecanica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 2000
                    , Projeto 3 "Eletrica" Eletrica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 3000
                    , Projeto 4 "Estrutural" Estrutural Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 4000
                    ]
        resultado = contarPorTipo projetos
        todosComUm = all (\(_, count) -> count == 1) resultado
        temQuatroTipos = length resultado == 4
    in todosComUm && temQuatroTipos

testeContarPorTipoDesbalanceado :: Bool
testeContarPorTipoDesbalanceado =
    let projetosCivil = replicate 10 (Projeto 1 "Civil" Civil Planejamento [] [] [] [] []
                                                (fromGregorian 2025 1 1) Nothing 1000)
        projetoMecanica = [Projeto 11 "Mecanica" Mecanica Planejamento [] [] [] [] []
                                    (fromGregorian 2025 1 1) Nothing 2000]
        projetos = projetosCivil ++ projetoMecanica
        resultado = contarPorTipo projetos
        civilCount = lookup Civil resultado
        mecanicaCount = lookup Mecanica resultado
    in civilCount == Just 10 && mecanicaCount == Just 1

testeContarPorTipoMuitosProjetos :: Bool
testeContarPorTipoMuitosProjetos =
    let projetos = [Projeto i ("P" ++ show i) (tiposPorId i) Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing (fromIntegral i * 1000)
                    | i <- [1..100]]
        resultado = contarPorTipo projetos
        totalContado = sum (map snd resultado)
    in totalContado == 100 && length resultado <= 4 -- m´aximo 4 tipos
    where
        tiposPorId i
            | i `mod` 4 == 1 = Civil
            | i `mod` 4 == 2 = Mecanica
            | i `mod` 4 == 3 = Eletrica
            | otherwise = Estrutural

testeContarPorTipoStatusVariado :: Bool
testeContarPorTipoStatusVariado =
    let projetos = [ Projeto 1 "A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 1000
                    , Projeto 2 "B" Civil Concluido [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 2000
                    , Projeto 3 "C" Civil Cancelado [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 3000
                    ]
        resultado = contarPorTipo projetos
        esperado = [(Civil, 3)] -- status n~ao importa, s´o tipo
    in resultado == esperado

-- =================================
-- TESTES PARA PROJETOS EM ATRASO
-- =================================

testeProjetosEmAtrasoNenhum :: Bool
testeProjetosEmAtrasoNenhum =
    let projetos = [ Projeto 1 "A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 12 1)) 1000
                    , Projeto 2 "B" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 2 1) (Just (fromGregorian 2025 12 1)) 2000
                    ]
        dataAtual = fromGregorian 2025 6 1 -- antes dos prazos
        resultado = projetosEmAtraso projetos dataAtual
    in null resultado

testeProjetosEmAtrasoTodos :: Bool
testeProjetosEmAtrasoTodos =
    let projetos = [ Projeto 1 "A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 3 1)) 1000
                    , Projeto 2 "B" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 2 1) (Just (fromGregorian 2025 4 1)) 2000
                    ]
        dataAtual = fromGregorian 2025 6 1 -- depois dos prazos
        resultado = projetosEmAtraso projetos dataAtual
    in length resultado == 2

testeProjetosEmAtrasoAlguns :: Bool
testeProjetosEmAtrasoAlguns =
    let projetos = [ Projeto 1 "Atrasado" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 3 1)) 1000
                    , Projeto 2 "No Prazo" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 2 1) (Just (fromGregorian 2025 12 1)) 2000
                    , Projeto 3 "Tamb´em Atrasado" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 4 1)) 3000
                    ]
        dataAtual = fromGregorian 2025 6 1
        resultado = projetosEmAtraso projetos dataAtual
        idsAtrasados = map idProjeto resultado
    in length resultado == 2 && 1 `elem` idsAtrasados && 3 `elem` idsAtrasados

testeProjetosEmAtrasoSemDataFim :: Bool
testeProjetosEmAtrasoSemDataFim =
    let projetos = [ Projeto 1 "Sem Prazo" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) Nothing 1000
                    , Projeto 2 "Com Prazo Vencido" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 2 1) (Just (fromGregorian 2025 3 1)) 2000
                    ]
        dataAtual = fromGregorian 2025 6 1
        resultado = projetosEmAtraso projetos dataAtual
    in length resultado == 1 && idProjeto (head resultado) == 2

testeProjetosEmAtrasoListaVazia :: Bool
testeProjetosEmAtrasoListaVazia =
    let projetos = []
        dataAtual = fromGregorian 2025 6 1
        resultado = projetosEmAtraso projetos dataAtual
    in null resultado

testeProjetosEmAtrasoPrazoHoje :: Bool
testeProjetosEmAtrasoPrazoHoje =
    let projetos = [ Projeto 1 "Prazo Hoje" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 6 1)) 1000]
        dataAtual = fromGregorian 2025 6 1 -- mesmo dia do prazo
        resultado = projetosEmAtraso projetos dataAtual
    in null resultado -- n~ao deve estar em atraso no mesmo dia

testeProjetosEmAtrasoPrazoAmanha :: Bool
testeProjetosEmAtrasoPrazoAmanha =
    let projetos = [ Projeto 1 "Prazo Amanh~a" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 6 2)) 1000]
        dataAtual = fromGregorian 2025 6 1 -- um dia antes do prazo
        resultado = projetosEmAtraso projetos dataAtual
    in null resultado

testeProjetosEmAtrasoProjetosConcluidos :: Bool
testeProjetosEmAtrasoProjetosConcluidos =
    let projetos = [ Projeto 1 "Conclu´ıdo no Prazo" Civil Concluido [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 3 1)) 1000
                    , Projeto 2 "Conclu´ıdo Atrasado" Civil Concluido [] [] [] [] []
                            (fromGregorian 2025 2 1) (Just (fromGregorian 2025 4 1)) 2000
                    , Projeto 3 "Em Andamento Atrasado" Civil EmDesenvolvimento [] [] [] [] (fromGregorian 2025 1 1) (Just (fromGregorian 2025 5 1)) 3000
                    ]
        dataAtual = fromGregorian 2025 6 1
        resultado = projetosEmAtraso projetos dataAtual
    in length resultado == 1 && idProjeto (head resultado) == 3 -- s´o o em andamento

testeProjetosEmAtrasoOrdenacao :: Bool
testeProjetosEmAtrasoOrdenacao =
    let projetos = [ Projeto 3 "C" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 5 1)) 3000
                    , Projeto 1 "A" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 3 1)) 1000
                    , Projeto 2 "B" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 4 1)) 2000
                    ]
        dataAtual = fromGregorian 2025 6 1
        resultado = projetosEmAtraso projetos dataAtual
        idsResultado = map idProjeto resultado
    in length resultado == 3 && idsResultado == [3, 1, 2] -- ordem original ou espec´ıfica

testeProjetosEmAtrasoTiposVariados :: Bool
testeProjetosEmAtrasoTiposVariados =
    let projetos = [ Projeto 1 "Civil Atrasado" Civil Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 3 1)) 1000
                    , Projeto 2 "Mec^anica Atrasada" Mecanica EmDesenvolvimento [] [] [] [] 61
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 4 1)) 2000
                    , Projeto 3 "El´etrica No Prazo" Eletrica Planejamento [] [] [] [] []
                            (fromGregorian 2025 1 1) (Just (fromGregorian 2025 12 1)) 3000
                    ]
        dataAtual = fromGregorian 2025 6 1
        resultado = projetosEmAtraso projetos dataAtual
        tiposAtrasados = map tipoProjeto resultado
    in length resultado == 2 && Civil `elem` tiposAtrasados && Mecanica `elem` tiposAtrasados

-- =================================
-- FUNC¸~OES AUXILIARES E EXECUC¸~AO
-- =================================

executarTestesEspecialista5 :: IO ()
executarTestesEspecialista5 = do
    putStrLn "======================================"
    putStrLn " TESTES ESPECIALISTA 5"
    putStrLn " (Valida¸c~ao, Relat´orios e Interface)"
    putStrLn "======================================"

    putStrLn "\n-- TESTES VALIDAR PROJETO --"
    putStrLn $ "Projeto v´alido: " ++ show testeValidarProjetoValido
    putStrLn $ "Nome vazio: " ++ show testeValidarProjetoNomeVazio
    putStrLn $ "Or¸camento negativo: " ++ show testeValidarProjetoOrcamentoNegativo
    putStrLn $ "Sem materiais: " ++ show testeValidarProjetoSemMateriais
    putStrLn $ "Data fim inv´alida: " ++ show testeValidarProjetoDataInvalidaFim
    putStrLn $ "ID negativo: " ++ show testeValidarProjetoIdNegativo
    putStrLn $ "Projeto completo: " ++ show testeValidarProjetoCompleto
    putStrLn $ "Material inv´alido: " ++ show testeValidarProjetoMaterialInvalido
    putStrLn $ "Or¸camento zero: " ++ show testeValidarProjetoOrcamentoZero
    putStrLn $ "Nome muito longo: " ++ show testeValidarProjetoNomeMuitoLongo

    putStrLn "\n-- TESTES CALCULAR CUSTO TOTAL --"
    putStrLn $ "B´asico: " ++ show testeCalcularCustoTotalBasico
    putStrLn $ "Sem materiais: " ++ show testeCalcularCustoTotalSemMateriais
    putStrLn $ "Um material: " ++ show testeCalcularCustoTotalUmMaterial
    putStrLn $ "Materiais caros: " ++ show testeCalcularCustoTotalMaterialesCaros
    putStrLn $ "Quantidade zero: " ++ show testeCalcularCustoTotalQuantidadeZero
    putStrLn $ "Decimais: " ++ show testeCalcularCustoTotalDecimais
    putStrLn $ "Muitos materiais: " ++ show testeCalcularCustoTotalMuitosMateriais
    putStrLn $ "Custo zero: " ++ show testeCalcularCustoTotalCustoZero
    putStrLn $ "Valores grandes: " ++ show testeCalcularCustoTotalValoresGrandes
    putStrLn $ "Precis~ao: " ++ show testeCalcularCustoTotalPrecisao

    putStrLn "\n-- TESTES GERAR RELAT´ORIO --"
    putStrLn $ "Completo: " ++ show testeGerarRelatorioProjetoCompleto
    putStrLn $ "M´ınimo: " ++ show testeGerarRelatorioProjetoMinimo
    putStrLn $ "Sem data fim: " ++ show testeGerarRelatorioProjetoSemDataFim
    putStrLn $ "Conclu´ıdo: " ++ show testeGerarRelatorioProjetoConcluido
    putStrLn $ "Muitos materiais: " ++ show testeGerarRelatorioProjetoMuitosMateriais
    putStrLn $ "Com c´alculos: " ++ show testeGerarRelatorioProjetoComCalculos
    putStrLn $ "Or¸camento alto: " ++ show testeGerarRelatorioProjetoOrcamentoAlto
    putStrLn $ "Tipos variados: " ++ show testeGerarRelatorioProjetoTiposVariados
    putStrLn $ "Formata¸c~ao: " ++ show testeGerarRelatorioProjetoFormatacao
    putStrLn $ "Com fun¸c~oes: " ++ show testeGerarRelatorioProjetoComFuncoes

    putStrLn "\n-- TESTES COMPARAR PROJETOS --"
    putStrLn $ "Iguais: " ++ show testeCompararProjetosIguais
    putStrLn $ "Tipos diferentes: " ++ show testeCompararProjetosDiferentesTipo
    putStrLn $ "Or¸camento diferente: " ++ show testeCompararProjetosOrcamentoDiferente
    putStrLn $ "Status diferente: " ++ show testeCompararProjetosStatusDiferente
    putStrLn $ "Datas diferentes: " ++ show testeCompararProjetosDatasDiferentes
    putStrLn $ "Materiais: " ++ show testeCompararProjetosMateriais
    putStrLn $ "Complexidade: " ++ show testeCompararProjetosComplexidade
    putStrLn $ "Nomes similares: " ++ show testeCompararProjetosNomesSimilares
    putStrLn $ "Com data fim: " ++ show testeCompararProjetosComDataFim
    putStrLn $ "Formata¸c~ao: " ++ show testeCompararProjetosFormatacao

    putStrLn "\n-- TESTES ESTAT´ISTICAS B´ASICAS --"
    putStrLn $ "Normal: " ++ show testeEstatisticasBasicasNormal
    putStrLn $ "Um elemento: " ++ show testeEstatisticasBasicasUmElemento
    putStrLn $ "Negativos: " ++ show testeEstatisticasBasicasNegativos
    putStrLn $ "Decimais: " ++ show testeEstatisticasBasicasDecimais
    putStrLn $ "Duplicados: " ++ show testeEstatisticasBasicasDuplicados
    putStrLn $ "Ordem aleat´oria: " ++ show testeEstatisticasBasicasOrdemAleatoria
    putStrLn $ "Grandes: " ++ show testeEstatisticasBasicasGrandes
    putStrLn $ "Pequenos: " ++ show testeEstatisticasBasicasPequenos
    putStrLn $ "Zeros: " ++ show testeEstatisticasBasicasZeros
    putStrLn $ "Muitos valores: " ++ show testeEstatisticasBasicasMuitosValores

    putStrLn "\n-- TESTES CONTAR POR TIPO --"
    putStrLn $ "´Unico: " ++ show testeContarPorTipoUnico
    putStrLn $ "V´ario: " ++ show testeContarPorTipoVario
    putStrLn $ "Vazio: " ++ show testeContarPorTipoVazio
    putStrLn $ "Todos iguais: " ++ show testeContarPorTipoTodosIguais
    putStrLn $ "Completo: " ++ show testeContarPorTipoCompleto
    putStrLn $ "Ordem: " ++ show testeContarPorTipoOrdem
    putStrLn $ "Um de cada tipo: " ++ show testeContarPorTipoUmDeCadaTipo
    putStrLn $ "Desbalanceado: " ++ show testeContarPorTipoDesbalanceado
    putStrLn $ "Muitos projetos: " ++ show testeContarPorTipoMuitosProjetos
    putStrLn $ "Status variado: " ++ show testeContarPorTipoStatusVariado

    putStrLn "\n-- TESTES PROJETOS EM ATRASO --"
    putStrLn $ "Nenhum: " ++ show testeProjetosEmAtrasoNenhum
    putStrLn $ "Todos: " ++ show testeProjetosEmAtrasoTodos
    putStrLn $ "Alguns: " ++ show testeProjetosEmAtrasoAlguns
    putStrLn $ "Sem data fim: " ++ show testeProjetosEmAtrasoSemDataFim
    putStrLn $ "Lista vazia: " ++ show testeProjetosEmAtrasoListaVazia
    putStrLn $ "Prazo hoje: " ++ show testeProjetosEmAtrasoPrazoHoje
    putStrLn $ "Prazo amanh~a: " ++ show testeProjetosEmAtrasoPrazoAmanha
    putStrLn $ "Projetos conclu´ıdos: " ++ show testeProjetosEmAtrasoProjetosConcluidos
    putStrLn $ "Ordena¸c~ao: " ++ show testeProjetosEmAtrasoOrdenacao
    putStrLn $ "Tipos variados: " ++ show testeProjetosEmAtrasoTiposVariados

testesEspecialista5 :: [(String, Bool)]
testesEspecialista5 =
    [ -- Validar Projeto
        ("Validar Projeto V´alido", testeValidarProjetoValido)
    , ("Validar Nome Vazio", testeValidarProjetoNomeVazio)
    , ("Validar Or¸camento Negativo", testeValidarProjetoOrcamentoNegativo)
    , ("Validar Sem Materiais", testeValidarProjetoSemMateriais)
    , ("Validar Data Fim Inv´alida", testeValidarProjetoDataInvalidaFim)
    , ("Validar ID Negativo", testeValidarProjetoIdNegativo)
    , ("Validar Projeto Completo", testeValidarProjetoCompleto)
    , ("Validar Material Inv´alido", testeValidarProjetoMaterialInvalido)
    , ("Validar Or¸camento Zero", testeValidarProjetoOrcamentoZero)
    , ("Validar Nome Muito Longo", testeValidarProjetoNomeMuitoLongo)

    -- Calcular Custo Total
    , ("Custo Total B´asico", testeCalcularCustoTotalBasico)
    , ("Custo Total Sem Materiais", testeCalcularCustoTotalSemMateriais)
    , ("Custo Total Um Material", testeCalcularCustoTotalUmMaterial)
    , ("Custo Total Materiais Caros", testeCalcularCustoTotalMaterialesCaros)
    , ("Custo Total Quantidade Zero", testeCalcularCustoTotalQuantidadeZero)
    , ("Custo Total Decimais", testeCalcularCustoTotalDecimais)
    , ("Custo Total Muitos Materiais", testeCalcularCustoTotalMuitosMateriais)
    , ("Custo Total Custo Zero", testeCalcularCustoTotalCustoZero)
    , ("Custo Total Valores Grandes", testeCalcularCustoTotalValoresGrandes)
    , ("Custo Total Precis~ao", testeCalcularCustoTotalPrecisao)

    -- Gerar Relat´orio
    , ("Relat´orio Completo", testeGerarRelatorioProjetoCompleto)
    , ("Relat´orio M´ınimo", testeGerarRelatorioProjetoMinimo)
    , ("Relat´orio Sem Data Fim", testeGerarRelatorioProjetoSemDataFim)
    , ("Relat´orio Conclu´ıdo", testeGerarRelatorioProjetoConcluido)
    , ("Relat´orio Muitos Materiais", testeGerarRelatorioProjetoMuitosMateriais)
    , ("Relat´orio Com C´alculos", testeGerarRelatorioProjetoComCalculos)
    , ("Relat´orio Or¸camento Alto", testeGerarRelatorioProjetoOrcamentoAlto)
    , ("Relat´orio Tipos Variados", testeGerarRelatorioProjetoTiposVariados)
    , ("Relat´orio Formata¸c~ao", testeGerarRelatorioProjetoFormatacao)
    , ("Relat´orio Com Fun¸c~oes", testeGerarRelatorioProjetoComFuncoes)

    -- Comparar Projetos
    , ("Comparar Projetos Iguais", testeCompararProjetosIguais)
    , ("Comparar Tipos Diferentes", testeCompararProjetosDiferentesTipo)
    , ("Comparar Or¸camento Diferente", testeCompararProjetosOrcamentoDiferente)
    , ("Comparar Status Diferente", testeCompararProjetosStatusDiferente)
    , ("Comparar Datas Diferentes", testeCompararProjetosDatasDiferentes)
    , ("Comparar Materiais", testeCompararProjetosMateriais)
    , ("Comparar Complexidade", testeCompararProjetosComplexidade)
    , ("Comparar Nomes Similares", testeCompararProjetosNomesSimilares)
    , ("Comparar Com Data Fim", testeCompararProjetosComDataFim)
    , ("Comparar Formata¸c~ao", testeCompararProjetosFormatacao)

    -- Estat´ısticas B´asicas
    , ("Estat´ısticas Normal", testeEstatisticasBasicasNormal)
    , ("Estat´ısticas Um Elemento", testeEstatisticasBasicasUmElemento)
    , ("Estat´ısticas Negativos", testeEstatisticasBasicasNegativos)
    , ("Estat´ısticas Decimais", testeEstatisticasBasicasDecimais)
    , ("Estat´ısticas Duplicados", testeEstatisticasBasicasDuplicados)
    , ("Estat´ısticas Ordem Aleat´oria", testeEstatisticasBasicasOrdemAleatoria)
    , ("Estat´ısticas Grandes", testeEstatisticasBasicasGrandes)
    , ("Estat´ısticas Pequenos", testeEstatisticasBasicasPequenos)
    , ("Estat´ısticas Zeros", testeEstatisticasBasicasZeros)
    , ("Estat´ısticas Muitos Valores", testeEstatisticasBasicasMuitosValores)

    -- Contar Por Tipo
    , ("Contar Tipo ´Unico", testeContarPorTipoUnico)
    , ("Contar Tipo V´ario", testeContarPorTipoVario)
    , ("Contar Tipo Vazio", testeContarPorTipoVazio)
    , ("Contar Tipo Todos Iguais", testeContarPorTipoTodosIguais)
    , ("Contar Tipo Completo", testeContarPorTipoCompleto)
    , ("Contar Tipo Ordem", testeContarPorTipoOrdem)
    , ("Contar Tipo Um de Cada", testeContarPorTipoUmDeCadaTipo)
    , ("Contar Tipo Desbalanceado", testeContarPorTipoDesbalanceado)
    , ("Contar Tipo Muitos Projetos", testeContarPorTipoMuitosProjetos)
    , ("Contar Tipo Status Variado", testeContarPorTipoStatusVariado)

    -- Projetos Em Atraso
    , ("Atraso Nenhum", testeProjetosEmAtrasoNenhum)
    , ("Atraso Todos", testeProjetosEmAtrasoTodos)
    , ("Atraso Alguns", testeProjetosEmAtrasoAlguns)
    , ("Atraso Sem Data Fim", testeProjetosEmAtrasoSemDataFim)
    , ("Atraso Lista Vazia", testeProjetosEmAtrasoListaVazia)
    , ("Atraso Prazo Hoje", testeProjetosEmAtrasoPrazoHoje)
    , ("Atraso Prazo Amanh~a", testeProjetosEmAtrasoPrazoAmanha)
    , ("Atraso Projetos Conclu´ıdos", testeProjetosEmAtrasoProjetosConcluidos)
    , ("Atraso Ordena¸c~ao", testeProjetosEmAtrasoOrdenacao)
    , ("Atraso Tipos Variados", testeProjetosEmAtrasoTiposVariados)
    ]


