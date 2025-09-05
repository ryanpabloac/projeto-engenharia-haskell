module Tests.ElectricalEngineeringTests where

import Types
import Engineering.Electrical

-- =================================
-- TESTES PARA LEI DE OHM (TENS~AO)
-- =================================

-- Testes b´asicos
testeLeiOhmBasica :: Bool
testeLeiOhmBasica =
    let corrente = 2 -- 2 A
        resistencia = 10 -- 10 §Ω§
        tensao = tensaoOhm corrente resistencia
    in abs (tensao - 20) < 0.001 -- V = I × R = 20V

testeLeiOhmLampada :: Bool
testeLeiOhmLampada =
    -- L^ampada incandescente t´ıpica: 0.5A em 240§Ω§
    let corrente = 0.5
        resistencia = 240
        tensao = tensaoOhm corrente resistencia
        esperada = 120 -- 120V (tens~ao dom´estica)
    in abs (tensao - esperada) < 0.001

testeLeiOhmResistorEletronico :: Bool
testeLeiOhmResistorEletronico =
    -- Resistor de 1k§Ω§ com 5mA
    let corrente = 0.005 -- 5 mA
        resistencia = 1000 -- 1 k§Ω§
        tensao = tensaoOhm corrente resistencia
        esperada = 5 -- 5V
    in abs (tensao - esperada) < 0.001

testeLeiOhmMotorEletrico :: Bool
testeLeiOhmMotorEletrico =
    -- Motor el´etrico: 15A, resist^encia interna 2§Ω§
    let corrente = 15
        resistencia = 2
        tensao = tensaoOhm corrente resistencia
        esperada = 30 -- 30V (queda de tens~ao interna)
    in abs (tensao - esperada) < 0.001

testeLeiOhmCorrenteZero :: Bool
testeLeiOhmCorrenteZero =
    let corrente = 0 -- sem corrente
        resistencia = 100
        tensao = tensaoOhm corrente resistencia
    in abs tensao < 0.001

testeLeiOhmCorrenteAlta :: Bool
testeLeiOhmCorrenteAlta =
    -- Soldador el´etrico: 50A, 0.1§Ω§
    let corrente = 50
        resistencia = 0.1
        tensao = tensaoOhm corrente resistencia
        esperada = 5 -- 5V
    in abs (tensao - esperada) < 0.001

testeLeiOhmMicroeletronica :: Bool
testeLeiOhmMicroeletronica =
    -- Circuito integrado: 1§μ§A, 1M§Ω§
    let corrente = 1e-6 -- 1 §μ§A
        resistencia = 1e6 -- 1 M§Ω§
        tensao = tensaoOhm corrente resistencia
        esperada = 1 -- 1V
    in abs (tensao - esperada) < 0.001

testeLeiOhmTransmissao :: Bool
testeLeiOhmTransmissao =
    -- Linha de transmiss~ao: 1000A, 0.01§Ω§
    let corrente = 1000
        resistencia = 0.01
        tensao = tensaoOhm corrente resistencia
        esperada = 10 -- 10V (perda na linha)
    in abs (tensao - esperada) < 0.001

testeLeiOhmDecimal :: Bool
testeLeiOhmDecimal =
    let corrente = 2.5
        resistencia = 4.7
        tensao = tensaoOhm corrente resistencia
        esperada = 11.75 -- 2.5 × 4.7
    in abs (tensao - esperada) < 0.001

testeLeiOhmPrecisao :: Bool
testeLeiOhmPrecisao =
    let corrente = 1.2345
        resistencia = 6.789
        tensao = tensaoOhm corrente resistencia
        esperada = corrente * resistencia
    in abs (tensao - esperada) < 0.001

-- =================================
-- TESTES PARA POT^ENCIA EL´ETRICA (V×I)
-- =================================

testePotenciaVIBasica :: Bool
testePotenciaVIBasica =
    let tensao = 12 -- 12 V
        corrente = 2 -- 2 A
        potencia = potenciaEletricaVI tensao corrente
    in abs (potencia - 24) < 0.001 -- P = V × I = 24W

testePotenciaVILED :: Bool
testePotenciaVILED =
    -- LED de alta pot^encia: 3.3V, 1A
    let tensao = 3.3
        corrente = 1.0
        potencia = potenciaEletricaVI tensao corrente
        esperada = 3.3 -- 3.3W
    in abs (potencia - esperada) < 0.001

testePotenciaVIChuveiro :: Bool
testePotenciaVIChuveiro =
    -- Chuveiro el´etrico: 220V, 25A
    let tensao = 220
        corrente = 25
        potencia = potenciaEletricaVI tensao corrente
        esperada = 5500 -- 5.5 kW
    in abs (potencia - esperada) < 1.0

testePotenciaVIMicroprocessador :: Bool
testePotenciaVIMicroprocessador =
    -- CPU: 1.2V, 50A (alta corrente, baixa tens~ao)
    let tensao = 1.2
        corrente = 50
        potencia = potenciaEletricaVI tensao corrente
        esperada = 60 -- 60W
    in abs (potencia - esperada) < 0.1

testePotenciaVICarregadorCelular :: Bool
testePotenciaVICarregadorCelular =
    -- Carregador: 5V, 2A
    let tensao = 5
        corrente = 2
        potencia = potenciaEletricaVI tensao corrente
        esperada = 10 -- 10W
    in abs (potencia - esperada) < 0.001

testePotenciaVIZero :: Bool
testePotenciaVIZero =
    let tensao = 10
        corrente = 0 -- sem corrente
        potencia = potenciaEletricaVI tensao corrente
    in abs potencia < 0.001

testePotenciaVIAltaTensao :: Bool
testePotenciaVIAltaTensao =
-- Sistema industrial: 13.8kV, 100A
    let tensao = 13800
        corrente = 100
        potencia = potenciaEletricaVI tensao corrente
        esperada = 1380000 -- 1.38 MW
    in abs (potencia - esperada) < 1000

testePotenciaVIBaixaPotencia :: Bool
testePotenciaVIBaixaPotencia =
    -- Sensor: 3V, 1mA
    let tensao = 3
        corrente = 0.001 -- 1 mA
        potencia = potenciaEletricaVI tensao corrente
        esperada = 0.003 -- 3 mW
    in abs (potencia - esperada) < 0.0001

testePotenciaVIDecimal :: Bool
testePotenciaVIDecimal =
    let tensao = 12.5
        corrente = 3.2
        potencia = potenciaEletricaVI tensao corrente
        esperada = 40.0 -- 12.5 × 3.2
    in abs (potencia - esperada) < 0.01

testePotenciaVIDispositivo9V :: Bool
testePotenciaVIDispositivo9V =
-- Exemplo do web search: 18W, 9V → 2A
    let tensao = 9
        corrente = 2 -- corrente calculada
        potencia = potenciaEletricaVI tensao corrente
        esperada = 18 -- 18W
    in abs (potencia - esperada) < 0.001

-- =================================
-- TESTES PARA POT^ENCIA RESISTIVA (R×I²)
-- =================================

testePotenciaRIBasica :: Bool
testePotenciaRIBasica =
    let resistencia = 10 -- 10 §Ω§
        corrente = 3 -- 3 A
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 90 -- P = R × I² = 90W
    in abs (potencia - esperada) < 0.001

testePotenciaRIResistorAquecimento :: Bool
testePotenciaRIResistorAquecimento =
    -- Resistor de aquecimento: 5§Ω§, 10A
    let resistencia = 5
        corrente = 10
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 500 -- 500W
    in abs (potencia - esperada) < 1.0

testePotenciaRIFilamentoLampada :: Bool
testePotenciaRIFilamentoLampada =
    -- Filamento de l^ampada: 240§Ω§, 0.5A
    let resistencia = 240
        corrente = 0.5
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 60 -- 60W
    in abs (potencia - esperada) < 0.1

testePotenciaRIFioTransmissao :: Bool
testePotenciaRIFioTransmissao =
    -- Perda em fio: 0.1§Ω§, 20A
    let resistencia = 0.1
        corrente = 20
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 40 -- 40W (perda)
    in abs (potencia - esperada) < 0.1

testePotenciaRICorrenteZero :: Bool
testePotenciaRICorrenteZero =
    let resistencia = 100
        corrente = 0 -- sem corrente
        potencia = potenciaEletricaRI resistencia corrente
    in abs potencia < 0.001

testePotenciaRIBaixaResistencia :: Bool
testePotenciaRIBaixaResistencia =
    -- Fus´ıvel: 0.001§Ω§, 10A
    let resistencia = 0.001
        corrente = 10
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 0.1 -- 0.1W
    in abs (potencia - esperada) < 0.001

testePotenciaRIAltaResistencia :: Bool
testePotenciaRIAltaResistencia =
    -- Resistor de alta imped^ancia: 1M§Ω§, 1mA
    let resistencia = 1e6
        corrente = 1e-3
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 1 -- 1W
    in abs (potencia - esperada) < 0.01

testePotenciaRIDecimal :: Bool
testePotenciaRIDecimal =
    let resistencia = 4.7
        corrente = 2.1
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 4.7 * 2.1^2 -- 4.7 × 4.41 = 20.727
    in abs (potencia - esperada) < 0.01

testePotenciaRITorradeira :: Bool
testePotenciaRITorradeira =
    -- Torradeira: 10§Ω§, 5A (exemplo do web search)
    let resistencia = 10
        corrente = 5
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 250 -- 250W
    in abs (potencia - esperada) < 1.0

testePotenciaRIFerroSolda :: Bool
testePotenciaRIFerroSolda =
    -- Ferro de solda: 20§Ω§, 2A
    let resistencia = 20
        corrente = 2
        potencia = potenciaEletricaRI resistencia corrente
        esperada = 80 -- 80W
    in abs (potencia - esperada) < 0.1

-- =================================
-- TESTES PARA POT^ENCIA POR TENS~AO (V²/R)
-- =================================

testePotenciaVRBasica :: Bool
testePotenciaVRBasica =
    let tensao = 20 -- 20 V
        resistencia = 4 -- 4 §Ω§
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 100 -- P = V²/R = 100W
    in abs (potencia - esperada) < 0.001

testePotenciaVRLampadaDomestica :: Bool
testePotenciaVRLampadaDomestica =
    -- L^ampada 60W, 120V
    let tensao = 120
        -- R = V²/P → R = 120²/60 = 240§Ω§
        resistencia = 240
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 60 -- 60W
    in abs (potencia - esperada) < 0.1

testePotenciaVRAquecedor :: Bool
testePotenciaVRAquecedor =
    -- Aquecedor el´etrico: 220V, 4.84§Ω§
    let tensao = 220
        resistencia = 4.84
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 220^2 / 4.84 -- ~10kW
    in abs (potencia - esperada) < 100

testePotenciaVRCircuitoEletronico :: Bool
testePotenciaVRCircuitoEletronico =
    -- Circuito 5V: resistor de 100§Ω§
    let tensao = 5
        resistencia = 100
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 0.25 -- 0.25W
    in abs (potencia - esperada) < 0.001

testePotenciaVRTensaoZero :: Bool
testePotenciaVRTensaoZero =
    let tensao = 0 -- sem tens~ao
        resistencia = 50
        potencia = potenciaEletricaVR tensao resistencia
    in abs potencia < 0.001

testePotenciaVRAltaTensao :: Bool
testePotenciaVRAltaTensao =
    -- Sistema de pot^encia: 13.8kV, 1000§Ω§
    let tensao = 13800
        resistencia = 1000
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 190440 -- ~190kW
    in abs (potencia - esperada) < 1000

testePotenciaVRResistorPequeno :: Bool
testePotenciaVRResistorPequeno =
    -- Resistor SMD: 3.3V, 1k§Ω§
    let tensao = 3.3
        resistencia = 1000
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 0.01089 -- ~10.9mW
    in abs (potencia - esperada) < 0.001

testePotenciaVRDecimal :: Bool
testePotenciaVRDecimal =
    let tensao = 15.5
        resistencia = 7.8
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 15.5^2 / 7.8
    in abs (potencia - esperada) < 0.01

testePotenciaVRSecadorCabelo :: Bool
testePotenciaVRSecadorCabelo =
    -- Secador: 220V, 20§Ω§ (exemplo baseado no web search)
    let tensao = 220
        resistencia = 20
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 2420 -- 2.42kW
    in abs (potencia - esperada) < 10

testePotenciaVRForno :: Bool
testePotenciaVRForno =
    -- Forno el´etrico: 240V, 19.2§Ω§
    let tensao = 240
        resistencia = 19.2
        potencia = potenciaEletricaVR tensao resistencia
        esperada = 3000 -- 3kW
    in abs (potencia - esperada) < 50

-- =================================
-- TESTES PARA RESIST^ENCIA EM S´ERIE
-- =================================

testeResistenciaSerieBasica :: Bool
testeResistenciaSerieBasica =
    let resistencias = [10, 20, 30] -- 10§Ω§, 20§Ω§, 30§Ω§
        total = resistenciaSerie resistencias
    in abs (total - 60) < 0.001 -- Rtotal = 60§Ω§

testeResistenciaSerieUmResistor :: Bool
testeResistenciaSerieUmResistor =
    let resistencias = [47] -- um resistor de 47§Ω§
        total = resistenciaSerie resistencias
    in abs (total - 47) < 0.001

testeResistenciaSerieVazia :: Bool
testeResistenciaSerieVazia =
    let resistencias = [] -- lista vazia
        total = resistenciaSerie resistencias
    in abs total < 0.001 -- deve ser zero

testeResistenciaSerieCircuitoLED :: Bool
testeResistenciaSerieCircuitoLED =
    -- LED + resistor limitador: 220§Ω§ + 100§Ω§
    let resistencias = [220, 100]
        total = resistenciaSerie resistencias
        esperada = 320 -- 320§Ω§
    in abs (total - esperada) < 0.001

testeResistenciaSerieDivisorTensao :: Bool
testeResistenciaSerieDivisorTensao =
    -- Divisor de tens~ao: 1k§Ω§ + 2k§Ω§ + 3k§Ω§
    let resistencias = [1000, 2000, 3000]
        total = resistenciaSerie resistencias
        esperada = 6000 -- 6k§Ω§
    in abs (total - esperada) < 0.001

testeResistenciaSerieAltosValores :: Bool
testeResistenciaSerieAltosValores =
    -- Resistores de alta imped^ancia: 1M§Ω§ + 2M§Ω§
    let resistencias = [1e6, 2e6]
        total = resistenciaSerie resistencias
        esperada = 3e6 -- 3M§Ω§
    in abs (total - esperada) < 1000

testeResistenciaSerieBaixosValores :: Bool
testeResistenciaSerieBaixosValores =
    -- Resistores de baixa imped^ancia: 0.1§Ω§ + 0.2§Ω§ + 0.3§Ω§
    let resistencias = [0.1, 0.2, 0.3]
        total = resistenciaSerie resistencias
        esperada = 0.6 -- 0.6§Ω§
    in abs (total - esperada) < 0.001

testeResistenciaSerieDecimais :: Bool
testeResistenciaSerieDecimais =
    let resistencias = [4.7, 2.2, 1.8, 6.8]
        total = resistenciaSerie resistencias
        esperada = 15.5 -- soma direta
    in abs (total - esperada) < 0.01

testeResistenciaSerieMuitosResistores :: Bool
testeResistenciaSerieMuitosResistores =
    -- 10 resistores de 100§Ω§ cada
    let resistencias = replicate 10 100
        total = resistenciaSerie resistencias
        esperada = 1000 -- 1k§Ω§
    in abs (total - esperada) < 0.1

testeResistenciaSerieValoresVariados :: Bool
testeResistenciaSerieValoresVariados =
    let resistencias = [0.47, 1.0, 2.2, 4.7, 10, 22, 47, 100]
        total = resistenciaSerie resistencias
        esperada = sum resistencias
    in abs (total - esperada) < 0.01

-- =================================
-- TESTES PARA RESIST^ENCIA EM PARALELO
-- =================================

testeResistenciaParaleloBasica :: Bool
testeResistenciaParaleloBasica =
    let resistencias = [20, 30] -- 20§Ω§, 30§Ω§ em paralelo
        total = resistenciaParalelo resistencias
        esperada = (20 * 30) / (20 + 30) -- 12§Ω§
    in abs (total - esperada) < 0.001

testeResistenciaParaleloIguais :: Bool
testeResistenciaParaleloIguais =
    -- Dois resistores iguais: 100§Ω§ || 100§Ω§
    let resistencias = [100, 100]
        total = resistenciaParalelo resistencias
        esperada = 50 -- metade do valor
    in abs (total - esperada) < 0.001

testeResistenciaParaleloTresIguais :: Bool
testeResistenciaParaleloTresIguais =
    -- Tr^es resistores iguais: 150§Ω§ || 150§Ω§ || 150§Ω§
    let resistencias = [150, 150, 150]
        total = resistenciaParalelo resistencias
        esperada = 50 -- R/3 = 150/3 = 50§Ω§
    in abs (total - esperada) < 0.001

testeResistenciaParaleloUmResistor :: Bool
testeResistenciaParaleloUmResistor =
    let resistencias = [75] -- um resistor
        total = resistenciaParalelo resistencias
    in abs (total - 75) < 0.001

testeResistenciaParaleloValoresAltos :: Bool
testeResistenciaParaleloValoresAltos =
    -- 1M§Ω§ || 2M§Ω§
    let resistencias = [1e6, 2e6]
        total = resistenciaParalelo resistencias
        esperada = (1e6 * 2e6) / (1e6 + 2e6) -- §≈§ 666.67k§Ω§
    in abs (total - esperada) < 1000

testeResistenciaParaleloBaixosValores :: Bool
testeResistenciaParaleloBaixosValores =
    -- 0.2§Ω§ || 0.3§Ω§
    let resistencias = [0.2, 0.3]
        total = resistenciaParalelo resistencias
        esperada = 0.12 -- 0.12§Ω§
    in abs (total - esperada) < 0.001

testeResistenciaParaleloAssimetrico :: Bool
testeResistenciaParaleloAssimetrico =
    -- 1§Ω§ || 1000§Ω§ (um muito menor que outro)
    let resistencias = [1, 1000]
        total = resistenciaParalelo resistencias
        -- Resultado pr´oximo do menor resistor
        esperada = 1000 / 1001 -- §≈§ 0.999§Ω§
    in abs (total - esperada) < 0.01

testeResistenciaParaleloQuatroResistores :: Bool
testeResistenciaParaleloQuatroResistores =
    -- 40§Ω§ || 60§Ω§ || 120§Ω§ || 200§Ω§
    let resistencias = [40, 60, 120, 200]
        total = resistenciaParalelo resistencias
        -- 1/Rp = 1/40 + 1/60 + 1/120 + 1/200
        reciprocoSoma = sum (map (1/) resistencias)
        esperada = 1 / reciprocoSoma
    in abs (total - esperada) < 0.1

testeResistenciaParaleloDecimais :: Bool
testeResistenciaParaleloDecimais =
    let resistencias = [4.7, 10, 22]
        total = resistenciaParalelo resistencias
        reciprocoSoma = (1/4.7) + (1/10) + (1/22)
        esperada = 1 / reciprocoSoma
    in abs (total - esperada) < 0.01

testeResistenciaParaleloCircuitoPratico :: Bool
testeResistenciaParaleloCircuitoPratico =
    -- Banco de resistores paralelos: 100§Ω§ || 220§Ω§ || 470§Ω§
    let resistencias = [100, 220, 470]
        total = resistenciaParalelo resistencias
        reciprocoSoma = (1/100) + (1/220) + (1/470)
        esperada = 1 / reciprocoSoma -- ~64.4§Ω§
    in abs (total - esperada) < 1.0

-- =================================
-- TESTES PARA IMPED^ANCIA AC
-- =================================

testeImpedanciaACBasica :: Bool
testeImpedanciaACBasica =
    let resistencia = 3 -- 3 §Ω§
        reatancia = 4 -- 4 §Ω§
        impedancia = impedanciaAC resistencia reatancia
        esperada = 5 -- Z = §√R + X =√9 + 16 = 5Ω§
    in abs (impedancia - esperada) < 0.001

testeImpedanciaACResistivoPuro :: Bool
testeImpedanciaACResistivoPuro =
    -- Circuito puramente resistivo (X = 0)
    let resistencia = 50
        reatancia = 0
        impedancia = impedanciaAC resistencia reatancia
        esperada = 50 -- Z = R quando X = 0
    in abs (impedancia - esperada) < 0.001

testeImpedanciaACReativoPuro :: Bool
testeImpedanciaACReativoPuro =
    -- Circuito puramente reativo (R = 0)
    let resistencia = 0
        reatancia = 25
        impedancia = impedanciaAC resistencia reatancia
        esperada = 25 -- Z = |X| quando R = 0
    in abs (impedancia - esperada) < 0.001

testeImpedanciaACMotorCA :: Bool
testeImpedanciaACMotorCA =
    -- Motor CA: R = 2§Ω§, X = 6§Ω§
    let resistencia = 2
        reatancia = 6
        impedancia = impedanciaAC resistencia reatancia
        esperada = sqrt (4 + 36) -- §√40 ≈ 6.32Ω§
    in abs (impedancia - esperada) < 0.01

testeImpedanciaACTransformador :: Bool
testeImpedanciaACTransformador =
    -- Transformador: R = 0.5§Ω§, X = 2.5 §Ω§
    let resistencia = 0.5
        reatancia = 2.5
        impedancia = impedanciaAC resistencia reatancia
        esperada = sqrt (0.25 + 6.25) -- §√6.5 ≈ 2.55Ω§
    in abs (impedancia - esperada) < 0.01

testeImpedanciaACCircuitoRLC :: Bool
testeImpedanciaACCircuitoRLC =
    -- Circuito RLC: R = 10§Ω§, X = 24§Ω§
    let resistencia = 10
        reatancia = 24
        impedancia = impedanciaAC resistencia reatancia
        esperada = 26 -- §√100 + 576 = 26Ω§
    in abs (impedancia - esperada) < 0.1

testeImpedanciaACReatanciaNegativa :: Bool
testeImpedanciaACReatanciaNegativa =
    -- Reat^ancia capacitiva (negativa): R = 8§Ω§, X = -6§Ω§
    let resistencia = 8
        reatancia = -6 -- capacitiva
        impedancia = impedanciaAC resistencia reatancia
        esperada = 10 -- §√64 + 36 = 10Ω§
    in abs (impedancia - esperada) < 0.001

testeImpedanciaACValoresAltos :: Bool
testeImpedanciaACValoresAltos =
    -- Alta imped^ancia: R = 1000§Ω§, X = 1500§Ω§
    let resistencia = 1000
        reatancia = 1500
        impedancia = impedanciaAC resistencia reatancia
        esperada = sqrt (1000000 + 2250000) -- §≈§ 1802.8§Ω§
    in abs (impedancia - esperada) < 1.0

testeImpedanciaACBaixaImpedancia :: Bool
testeImpedanciaACBaixaImpedancia =
    -- Baixa imped^ancia: R = 0.1§Ω§, X = 0.2§Ω§
    let resistencia = 0.1
        reatancia = 0.2
        impedancia = impedanciaAC resistencia reatancia
        esperada = sqrt (0.01 + 0.04) -- §√0.05 ≈ 0.224Ω§
    in abs (impedancia - esperada) < 0.001

testeImpedanciaACDecimal :: Bool
testeImpedanciaACDecimal =
    let resistencia = 12.5
        reatancia = 9.3
        impedancia = impedanciaAC resistencia reatancia
        esperada = sqrt (12.5^2 + 9.3^2)
    in abs (impedancia - esperada) < 0.01

-- =================================
-- TESTES PARA CONVERS~OES POLARES/RETANGULARES
-- =================================

testePolarParaRetangularBasico :: Bool
testePolarParaRetangularBasico =
    let modulo = 5 -- 5
        angulo = pi/3 -- 60°
        (x, y) = polarParaRetangular modulo angulo
        xEsperado = 5 * cos(pi/3) -- 2.5
        yEsperado = 5 * sin(pi/3) -- 4.33
    in abs (x - xEsperado) < 0.01 && abs (y - yEsperado) < 0.01

testePolarParaRetangularAngulo0 :: Bool
testePolarParaRetangularAngulo0 =
    -- ^Angulo 0° (eixo real positivo)
    let modulo = 10
        angulo = 0
        (x, y) = polarParaRetangular modulo angulo
    in abs (x - 10) < 0.001 && abs y < 0.001

testePolarParaRetangularAngulo90 :: Bool
testePolarParaRetangularAngulo90 =
    -- ^Angulo 90° (eixo imagin´ario positivo)
    let modulo = 8
        angulo = pi/2
        (x, y) = polarParaRetangular modulo angulo
    in abs x < 0.001 && abs (y - 8) < 0.001

testePolarParaRetangularAngulo180 :: Bool
testePolarParaRetangularAngulo180 =
    -- ^Angulo 180° (eixo real negativo)
    let modulo = 6
        angulo = pi
        (x, y) = polarParaRetangular modulo angulo
    in abs (x - (-6)) < 0.01 && abs y < 0.01

testePolarParaRetangularAngulo270 :: Bool
testePolarParaRetangularAngulo270 =
    -- ^Angulo 270° (eixo imagin´ario negativo)
    let modulo = 4
        angulo = 3*pi/2
        (x, y) = polarParaRetangular modulo angulo
    in abs x < 0.01 && abs (y - (-4)) < 0.01

testeRetangularParaPolarBasico :: Bool
testeRetangularParaPolarBasico =
    let x = 3
        y = 4
        (r, theta) = retangularParaPolar x y
        rEsperado = 5 -- §√3 + 4 = 5§
        thetaEsperado = atan2 4 3 -- atan(4/3)
    in abs (r - rEsperado) < 0.001 && abs (theta - thetaEsperado) < 0.001

testeRetangularParaPolarEixoReal :: Bool
testeRetangularParaPolarEixoReal =
    -- Ponto no eixo real positivo
    let x = 7
        y = 0
        (r, theta) = retangularParaPolar x y
    in abs (r - 7) < 0.001 && abs theta < 0.001

testeRetangularParaPolarEixoImaginario :: Bool
testeRetangularParaPolarEixoImaginario =
    -- Ponto no eixo imagin´ario positivo
    let x = 0
        y = 9
        (r, theta) = retangularParaPolar x y
    in abs (r - 9) < 0.001 && abs (theta - pi/2) < 0.01

testeRetangularParaPolarQuadrante2 :: Bool
testeRetangularParaPolarQuadrante2 =
    -- Segundo quadrante
    let x = -3
        y = 4
        (r, theta) = retangularParaPolar x y
        rEsperado = 5
        thetaEsperado = atan2 4 (-3) -- ^angulo no 2° quadrante
    in abs (r - rEsperado) < 0.001 && abs (theta - thetaEsperado) < 0.01

testeRetangularParaPolarQuadrante3 :: Bool
testeRetangularParaPolarQuadrante3 =
    -- Terceiro quadrante
    let x = -5
        y = -12
        (r, theta) = retangularParaPolar x y
        rEsperado = 13 -- §√25 + 144 = 13§
        thetaEsperado = atan2 (-12) (-5)
    in abs (r - rEsperado) < 0.001 && abs (theta - thetaEsperado) < 0.01

testeConversaoRoundTrip :: Bool
testeConversaoRoundTrip =
    -- Teste ida e volta
    let rOriginal = 10
        thetaOriginal = pi/4
        (x, y) = polarParaRetangular rOriginal thetaOriginal
        (rFinal, thetaFinal) = retangularParaPolar x y
    in abs (rOriginal - rFinal) < 0.001 &&
        abs (thetaOriginal - thetaFinal) < 0.001

testeConversaoRoundTripComplexo :: Bool
testeConversaoRoundTripComplexo =
    -- Teste com valores complexos
    let xOriginal = -7.5
        yOriginal = 2.3
        (r, theta) = retangularParaPolar xOriginal yOriginal
        (xFinal, yFinal) = polarParaRetangular r theta
    in abs (xOriginal - xFinal) < 0.01 && abs (yOriginal - yFinal) < 0.01

testePolarParaRetangularFasor :: Bool
testePolarParaRetangularFasor =
    -- Fasor el´etrico: 100§̸ §30°
    let modulo = 100
        angulo = pi/6 -- 30°
        (x, y) = polarParaRetangular modulo angulo
        xEsperado = 100 * cos(pi/6) -- §≈§ 86.6
        yEsperado = 100 * sin(pi/6) -- 50
    in abs (x - xEsperado) < 0.1 && abs (y - yEsperado) < 0.1

testeRetangularParaPolarImpedancia :: Bool
testeRetangularParaPolarImpedancia =
    -- Imped^ancia complexa: Z = 6 + j8
    let x = 6 -- resist^encia
        y = 8 -- reat^ancia
        (r, theta) = retangularParaPolar x y
        rEsperado = 10 -- |Z| = 10§Ω§
        thetaEsperado = atan2 8 6 -- ^angulo de fase
    in abs (r - rEsperado) < 0.1 && abs (theta - thetaEsperado) < 0.01

testeConversaoOrigemZero :: Bool
testeConversaoOrigemZero =
    let x = 0
        y = 0
        (r, theta) = retangularParaPolar x y
    in abs r < 0.001 -- m´odulo deve ser zero (^angulo indefinido)

-- =================================
-- TESTES INTEGRADOS E CASOS REAIS
-- =================================

testeCasoRealCircuitoResidencial :: Bool
testeCasoRealCircuitoResidencial =
    -- Circuito residencial: l^ampadas em paralelo
    let lampadaW = 60 -- 60W cada
        tensao = 120 -- 120V
        resistenciaUmaLampada = tensao^2 / lampadaW -- R = V²/P = 240§Ω§
        resistencias = replicate 5 resistenciaUmaLampada -- 5 l^ampadas
        resistenciaTotal = resistenciaParalelo resistencias
        correnteTotal = tensao / resistenciaTotal
        potenciaTotal = potenciaEletricaVI tensao correnteTotal
        -- Verifica¸c~oes
        resistenciaOk = resistenciaTotal < 50 -- menor que individual
        potenciaOk = abs (potenciaTotal - 300) < 10 -- 5 × 60W = 300W
    in resistenciaOk && potenciaOk

testeCasoRealMotorTrifasico :: Bool
testeCasoRealMotorTrifasico =
    -- Motor trif´asico industrial
    let tensao = 380 -- 380V
        resistencia = 0.5 -- 0.5§Ω§ por fase
        reatancia = 2.0 -- 2§Ω§ reat^ancia
        impedancia = impedanciaAC resistencia reatancia
        corrente = tensao / impedancia
        potenciaAparente = potenciaEletricaVI tensao corrente
        -- Convers~ao para forma retangular/polar
        (x, y) = (resistencia, reatancia)
        (r, theta) = retangularParaPolar x y
        -- Verifica¸c~oes
        impedanciaOk = impedancia > 2 && impedancia < 3 -- ~2.06§Ω§
        anguloOk = theta > 1.0 && theta < 1.4 -- ~76° em radianos
    in impedanciaOk && anguloOk

testeCasoRealDivisorTensao :: Bool
testeCasoRealDivisorTensao =
    -- Divisor de tens~ao para sensor
    let tensaoFonte = 12 -- 12V
        r1 = 10000 -- 10k§Ω§
        r2 = 5000 -- 5k§Ω§ (sensor)
        resistenciaTotal = resistenciaSerie [r1, r2]
        correnteCircuito = tensaoFonte / resistenciaTotal
        tensaoSensor = tensaoOhm correnteCircuito r2
        potenciaSensor = potenciaEletricaRI r2 correnteCircuito
        -- Verifica¸c~oes
        tensaoOk = abs (tensaoSensor - 4) < 0.1 -- 1/3 de 12V = 4V
        potenciaOk = potenciaSensor < 0.005 -- menos de 5mW
    in tensaoOk && potenciaOk

testeCasoRealCarregadorCelular :: Bool
testeCasoRealCarregadorCelular =
    -- Carregador USB-C: 20V, 3A
    let tensao = 20
        corrente = 3
        potenciaSaida = potenciaEletricaVI tensao corrente
        resistenciaInterna = 0.1 -- 0.1§Ω§ resist^encia interna
        perdaInterna = potenciaEletricaRI resistenciaInterna corrente
        potenciaEntrada = potenciaSaida + perdaInterna
        eficiencia = potenciaSaida / potenciaEntrada
        -- Verifica¸c~oes
        potenciaOk = abs (potenciaSaida - 60) < 1 -- 60W
        eficienciaOk = eficiencia > 0.98 -- > 98% efici^encia
    in potenciaOk && eficienciaOk

testeCasoRealFiltroPassaBaixa :: Bool
testeCasoRealFiltroPassaBaixa =
    -- Filtro RC passa-baixa: R=1k§Ω§, f=1kHz, C=159nF
    let resistencia = 1000
        frequencia = 1000 -- 1kHz
        capacitancia = 159e-9 -- 159nF
        reatanciaCapacitiva = 1 / (2 * pi * frequencia * capacitancia)
        impedancia = impedanciaAC resistencia reatanciaCapacitiva
        -- Convers~ao para an´alise fasorial
        (r, theta) = retangularParaPolar resistencia reatanciaCapacitiva
        -- Verifica¸c~oes (reat^ancia capacitiva ´e negativa, mas usamos m´odulo)
        impedanciaOk = impedancia > 1000 && impedancia < 1500 -- ~1414§Ω§
        anguloOk = abs theta > 0.7 && abs theta < 0.9 -- ~45° = §π§/4 rad
    in impedanciaOk && anguloOk

-- =================================
-- FUNC¸~AO DE EXECUC¸~AO DOS TESTES
-- =================================

executarTestesEngenhariaEletrica :: IO ()
executarTestesEngenhariaEletrica = do
    putStrLn "======================================"
    putStrLn " TESTES ENGENHARIA EL´ETRICA"
    putStrLn "======================================"

    putStrLn "\n-- TESTES LEI DE OHM --"
    putStrLn $ "B´asica: " ++ show testeLeiOhmBasica
    putStrLn $ "L^ampada: " ++ show testeLeiOhmLampada
    putStrLn $ "Resistor eletr^onico: " ++ show testeLeiOhmResistorEletronico
    putStrLn $ "Motor el´etrico: " ++ show testeLeiOhmMotorEletrico
    putStrLn $ "Corrente zero: " ++ show testeLeiOhmCorrenteZero
    putStrLn $ "Corrente alta: " ++ show testeLeiOhmCorrenteAlta
    putStrLn $ "Microeletr^onica: " ++ show testeLeiOhmMicroeletronica
    putStrLn $ "Transmiss~ao: " ++ show testeLeiOhmTransmissao
    putStrLn $ "Decimal: " ++ show testeLeiOhmDecimal
    putStrLn $ "Precis~ao: " ++ show testeLeiOhmPrecisao

    putStrLn "\n-- TESTES POT^ENCIA V×I --"
    putStrLn $ "B´asica: " ++ show testePotenciaVIBasica
    putStrLn $ "LED: " ++ show testePotenciaVILED
    putStrLn $ "Chuveiro: " ++ show testePotenciaVIChuveiro
    putStrLn $ "Microprocessador: " ++ show testePotenciaVIMicroprocessador
    putStrLn $ "Carregador celular: " ++ show testePotenciaVICarregadorCelular
    putStrLn $ "Zero: " ++ show testePotenciaVIZero
    putStrLn $ "Alta tens~ao: " ++ show testePotenciaVIAltaTensao
    putStrLn $ "Baixa pot^encia: " ++ show testePotenciaVIBaixaPotencia
    putStrLn $ "Decimal: " ++ show testePotenciaVIDecimal
    putStrLn $ "Dispositivo 9V: " ++ show testePotenciaVIDispositivo9V

    putStrLn "\n-- TESTES POT^ENCIA R×I² --"
    putStrLn $ "B´asica: " ++ show testePotenciaRIBasica
    putStrLn $ "Resistor aquecimento: " ++ show testePotenciaRIResistorAquecimento
    putStrLn $ "Filamento l^ampada: " ++ show testePotenciaRIFilamentoLampada
    putStrLn $ "Fio transmiss~ao: " ++ show testePotenciaRIFioTransmissao
    putStrLn $ "Corrente zero: " ++ show testePotenciaRICorrenteZero
    putStrLn $ "Baixa resist^encia: " ++ show testePotenciaRIBaixaResistencia
    putStrLn $ "Alta resist^encia: " ++ show testePotenciaRIAltaResistencia
    putStrLn $ "Decimal: " ++ show testePotenciaRIDecimal
    putStrLn $ "Torradeira: " ++ show testePotenciaRITorradeira
    putStrLn $ "Ferro solda: " ++ show testePotenciaRIFerroSolda

    putStrLn "\n-- TESTES POT^ENCIA V²/R --"
    putStrLn $ "B´asica: " ++ show testePotenciaVRBasica
    putStrLn $ "L^ampada dom´estica: " ++ show testePotenciaVRLampadaDomestica
    putStrLn $ "Aquecedor: " ++ show testePotenciaVRAquecedor
    putStrLn $ "Circuito eletr^onico: " ++ show testePotenciaVRCircuitoEletronico
    putStrLn $ "Tens~ao zero: " ++ show testePotenciaVRTensaoZero
    putStrLn $ "Alta tens~ao: " ++ show testePotenciaVRAltaTensao
    putStrLn $ "Resistor pequeno: " ++ show testePotenciaVRResistorPequeno
    putStrLn $ "Decimal: " ++ show testePotenciaVRDecimal
    putStrLn $ "Secador cabelo: " ++ show testePotenciaVRSecadorCabelo
    putStrLn $ "Forno: " ++ show testePotenciaVRForno

    putStrLn "\n-- TESTES RESIST^ENCIA S´ERIE --"
    putStrLn $ "B´asica: " ++ show testeResistenciaSerieBasica
    putStrLn $ "Um resistor: " ++ show testeResistenciaSerieUmResistor
    putStrLn $ "Vazia: " ++ show testeResistenciaSerieVazia
    putStrLn $ "Circuito LED: " ++ show testeResistenciaSerieCircuitoLED
    putStrLn $ "Divisor tens~ao: " ++ show testeResistenciaSerieDivisorTensao
    putStrLn $ "Altos valores: " ++ show testeResistenciaSerieAltosValores
    putStrLn $ "Baixos valores: " ++ show testeResistenciaSerieBaixosValores
    putStrLn $ "Decimais: " ++ show testeResistenciaSerieDecimais
    putStrLn $ "Muitos resistores: " ++ show testeResistenciaSerieMuitosResistores
    putStrLn $ "Valores variados: " ++ show testeResistenciaSerieValoresVariados

    putStrLn "\n-- TESTES RESIST^ENCIA PARALELO --"
    putStrLn $ "B´asica: " ++ show testeResistenciaParaleloBasica
    putStrLn $ "Iguais: " ++ show testeResistenciaParaleloIguais
    putStrLn $ "Tr^es iguais: " ++ show testeResistenciaParaleloTresIguais
    putStrLn $ "Um resistor: " ++ show testeResistenciaParaleloUmResistor
    putStrLn $ "Valores altos: " ++ show testeResistenciaParaleloValoresAltos
    putStrLn $ "Baixos valores: " ++ show testeResistenciaParaleloBaixosValores
    putStrLn $ "Assim´etrico: " ++ show testeResistenciaParaleloAssimetrico
    putStrLn $ "Quatro resistores: " ++ show testeResistenciaParaleloQuatroResistores
    putStrLn $ "Decimais: " ++ show testeResistenciaParaleloDecimais
    putStrLn $ "Circuito pr´atico: " ++ show testeResistenciaParaleloCircuitoPratico

    putStrLn "\n-- TESTES IMPED^ANCIA AC --"
    putStrLn $ "B´asica: " ++ show testeImpedanciaACBasica
    putStrLn $ "Resistivo puro: " ++ show testeImpedanciaACResistivoPuro
    putStrLn $ "Reativo puro: " ++ show testeImpedanciaACReativoPuro
    putStrLn $ "Motor CA: " ++ show testeImpedanciaACMotorCA
    putStrLn $ "Transformador: " ++ show testeImpedanciaACTransformador
    putStrLn $ "Circuito RLC: " ++ show testeImpedanciaACCircuitoRLC
    putStrLn $ "Reat^ancia negativa: " ++ show testeImpedanciaACReatanciaNegativa
    putStrLn $ "Valores altos: " ++ show testeImpedanciaACValoresAltos
    putStrLn $ "Baixa imped^ancia: " ++ show testeImpedanciaACBaixaImpedancia
    putStrLn $ "Decimal: " ++ show testeImpedanciaACDecimal

    putStrLn "\n-- TESTES CONVERS~OES COORDENADAS --"
    putStrLn $ "Polar→Retangular b´asico: " ++ show testePolarParaRetangularBasico
    putStrLn $ "^Angulo 0°: " ++ show testePolarParaRetangularAngulo0
    putStrLn $ "^Angulo 90°: " ++ show testePolarParaRetangularAngulo90
    putStrLn $ "^Angulo 180°: " ++ show testePolarParaRetangularAngulo180
    putStrLn $ "^Angulo 270°: " ++ show testePolarParaRetangularAngulo270
    putStrLn $ "Retangular→Polar b´asico: " ++ show testeRetangularParaPolarBasico
    putStrLn $ "Eixo real: " ++ show testeRetangularParaPolarEixoReal
    putStrLn $ "Eixo imagin´ario: " ++ show testeRetangularParaPolarEixoImaginario
    putStrLn $ "2° quadrante: " ++ show testeRetangularParaPolarQuadrante2
    putStrLn $ "3° quadrante: " ++ show testeRetangularParaPolarQuadrante3
    putStrLn $ "Round trip: " ++ show testeConversaoRoundTrip
    putStrLn $ "Round trip complexo: " ++ show testeConversaoRoundTripComplexo
    putStrLn $ "Fasor el´etrico: " ++ show testePolarParaRetangularFasor
    putStrLn $ "Imped^ancia complexa: " ++ show testeRetangularParaPolarImpedancia
    putStrLn $ "Origem zero: " ++ show testeConversaoOrigemZero

    putStrLn "\n-- TESTES CASOS REAIS --"
    putStrLn $ "Circuito residencial: " ++ show testeCasoRealCircuitoResidencial
    putStrLn $ "Motor trif´asico: " ++ show testeCasoRealMotorTrifasico
    putStrLn $ "Divisor tens~ao: " ++ show testeCasoRealDivisorTensao
    putStrLn $ "Carregador celular: " ++ show testeCasoRealCarregadorCelular
    putStrLn $ "Filtro passa-baixa: " ++ show testeCasoRealFiltroPassaBaixa

testesEngenhariaEletrica :: [(String, Bool)]
testesEngenhariaEletrica =
    [ -- Lei de Ohm (10 testes)
        ("Lei Ohm B´asica", testeLeiOhmBasica)
    , ("Lei Ohm L^ampada", testeLeiOhmLampada)
    , ("Lei Ohm Resistor Eletr^onico", testeLeiOhmResistorEletronico)
    , ("Lei Ohm Motor El´etrico", testeLeiOhmMotorEletrico)
    , ("Lei Ohm Corrente Zero", testeLeiOhmCorrenteZero)
    , ("Lei Ohm Corrente Alta", testeLeiOhmCorrenteAlta)
    , ("Lei Ohm Microeletr^onica", testeLeiOhmMicroeletronica)
    , ("Lei Ohm Transmiss~ao", testeLeiOhmTransmissao)
    , ("Lei Ohm Decimal", testeLeiOhmDecimal)
    , ("Lei Ohm Precis~ao", testeLeiOhmPrecisao)

    -- Pot^encia V×I (10 testes)
    , ("Pot^encia VI B´asica", testePotenciaVIBasica)
    , ("Pot^encia VI LED", testePotenciaVILED)
    , ("Pot^encia VI Chuveiro", testePotenciaVIChuveiro)
    , ("Pot^encia VI Microprocessador", testePotenciaVIMicroprocessador)
    , ("Pot^encia VI Carregador", testePotenciaVICarregadorCelular)
    , ("Pot^encia VI Zero", testePotenciaVIZero)
    , ("Pot^encia VI Alta Tens~ao", testePotenciaVIAltaTensao)
    , ("Pot^encia VI Baixa", testePotenciaVIBaixaPotencia)
    , ("Pot^encia VI Decimal", testePotenciaVIDecimal)
    , ("Pot^encia VI Dispositivo 9V", testePotenciaVIDispositivo9V)

    -- Pot^encia R×I² (10 testes)
    , ("Pot^encia RI B´asica", testePotenciaRIBasica)
    , ("Pot^encia RI Aquecimento", testePotenciaRIResistorAquecimento)
    , ("Pot^encia RI Filamento", testePotenciaRIFilamentoLampada)
    , ("Pot^encia RI Fio", testePotenciaRIFioTransmissao)
    , ("Pot^encia RI Corrente Zero", testePotenciaRICorrenteZero)
    , ("Pot^encia RI Baixa R", testePotenciaRIBaixaResistencia)
    , ("Pot^encia RI Alta R", testePotenciaRIAltaResistencia)
    , ("Pot^encia RI Decimal", testePotenciaRIDecimal)
    , ("Pot^encia RI Torradeira", testePotenciaRITorradeira)
    , ("Pot^encia RI Ferro Solda", testePotenciaRIFerroSolda)

    -- Pot^encia V²/R (10 testes)
    , ("Pot^encia VR B´asica", testePotenciaVRBasica)
    , ("Pot^encia VR L^ampada", testePotenciaVRLampadaDomestica)
    , ("Pot^encia VR Aquecedor", testePotenciaVRAquecedor)
    , ("Pot^encia VR Eletr^onico", testePotenciaVRCircuitoEletronico)
    , ("Pot^encia VR Tens~ao Zero", testePotenciaVRTensaoZero)
    , ("Pot^encia VR Alta Tens~ao", testePotenciaVRAltaTensao)
    , ("Pot^encia VR Resistor Pequeno", testePotenciaVRResistorPequeno)
    , ("Pot^encia VR Decimal", testePotenciaVRDecimal)
    , ("Pot^encia VR Secador", testePotenciaVRSecadorCabelo)
    , ("Pot^encia VR Forno", testePotenciaVRForno)

    -- Resist^encia S´erie (10 testes)
    , ("Resist^encia S´erie B´asica", testeResistenciaSerieBasica)
    , ("Resist^encia S´erie Um", testeResistenciaSerieUmResistor)
    , ("Resist^encia S´erie Vazia", testeResistenciaSerieVazia)
    , ("Resist^encia S´erie LED", testeResistenciaSerieCircuitoLED)
    , ("Resist^encia S´erie Divisor", testeResistenciaSerieDivisorTensao)
    , ("Resist^encia S´erie Alta", testeResistenciaSerieAltosValores)
    , ("Resist^encia S´erie Baixa", testeResistenciaSerieBaixosValores)
    , ("Resist^encia S´erie Decimal", testeResistenciaSerieDecimais)
    , ("Resist^encia S´erie Muitos", testeResistenciaSerieMuitosResistores)
    , ("Resist^encia S´erie Variados", testeResistenciaSerieValoresVariados)

    -- Resist^encia Paralelo (10 testes)
    , ("Resist^encia Paralelo B´asica", testeResistenciaParaleloBasica)
    , ("Resist^encia Paralelo Iguais", testeResistenciaParaleloIguais)
    , ("Resist^encia Paralelo Tr^es", testeResistenciaParaleloTresIguais)
    , ("Resist^encia Paralelo Um", testeResistenciaParaleloUmResistor)
    , ("Resist^encia Paralelo Alta", testeResistenciaParaleloValoresAltos)
    , ("Resist^encia Paralelo Baixa", testeResistenciaParaleloBaixosValores)
    , ("Resist^encia Paralelo Assim´etrico", testeResistenciaParaleloAssimetrico)
    , ("Resist^encia Paralelo Quatro", testeResistenciaParaleloQuatroResistores)
    , ("Resist^encia Paralelo Decimal", testeResistenciaParaleloDecimais)
    , ("Resist^encia Paralelo Pr´atico", testeResistenciaParaleloCircuitoPratico)

    -- Imped^ancia AC (10 testes)
    , ("Imped^ancia AC B´asica", testeImpedanciaACBasica)
    , ("Imped^ancia AC Resistivo", testeImpedanciaACResistivoPuro)
    , ("Imped^ancia AC Reativo", testeImpedanciaACReativoPuro)
    , ("Imped^ancia AC Motor", testeImpedanciaACMotorCA)
    , ("Imped^ancia AC Transformador", testeImpedanciaACTransformador)
    , ("Imped^ancia AC RLC", testeImpedanciaACCircuitoRLC)
    , ("Imped^ancia AC Negativa", testeImpedanciaACReatanciaNegativa)
    , ("Imped^ancia AC Alta", testeImpedanciaACValoresAltos)
    , ("Imped^ancia AC Baixa", testeImpedanciaACBaixaImpedancia)
    , ("Imped^ancia AC Decimal", testeImpedanciaACDecimal)

    -- Convers~oes Coordenadas (15 testes)
    , ("Polar→Retangular B´asico", testePolarParaRetangularBasico)
    , ("Polar→Retangular 0°", testePolarParaRetangularAngulo0)
    , ("Polar→Retangular 90°", testePolarParaRetangularAngulo90)
    , ("Polar→Retangular 180°", testePolarParaRetangularAngulo180)
    , ("Polar→Retangular 270°", testePolarParaRetangularAngulo270)
    , ("Retangular→Polar B´asico", testeRetangularParaPolarBasico)
    , ("Retangular→Polar Eixo Real", testeRetangularParaPolarEixoReal)
    , ("Retangular→Polar Eixo Imag", testeRetangularParaPolarEixoImaginario)
    , ("Retangular→Polar Q2", testeRetangularParaPolarQuadrante2)
    , ("Retangular→Polar Q3", testeRetangularParaPolarQuadrante3)
    , ("Convers~ao Round Trip", testeConversaoRoundTrip)
    , ("Convers~ao Round Trip Complexo", testeConversaoRoundTripComplexo)
    , ("Polar→Retangular Fasor", testePolarParaRetangularFasor)
    , ("Retangular→Polar Imped^ancia", testeRetangularParaPolarImpedancia)
    , ("Convers~ao Origem Zero", testeConversaoOrigemZero)

    -- Casos Reais (5 testes)
    , ("Caso Real Residencial", testeCasoRealCircuitoResidencial)
    , ("Caso Real Motor Trif´asico", testeCasoRealMotorTrifasico)
    , ("Caso Real Divisor Tens~ao", testeCasoRealDivisorTensao)
    , ("Caso Real Carregador", testeCasoRealCarregadorCelular)
    , ("Caso Real Filtro", testeCasoRealFiltroPassaBaixa)
    ]
