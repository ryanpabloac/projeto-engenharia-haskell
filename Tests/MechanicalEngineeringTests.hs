module Tests.MechanicalEngineeringTests where

import Types
import Engineering.Mechanical

-- =================================
-- TESTES PARA CALCULAR TORQUE
-- =================================
-- Testes b´asicos

testeTorqueBasico :: Bool
testeTorqueBasico =
  let forca = 100 -- 100 N
    raio = 0.5 -- 0.5 m
    angulo = pi/2 -- 90° (for¸ca perpendicular)
    torque = calcularTorque forca raio angulo
    esperado = 100 * 0.5 * sin(pi/2) -- 50 §N · m§
  in abs (torque - esperado) < 0.001

testeTorqueForcaPerpendicular :: Bool
testeTorqueForcaPerpendicular =
  let forca = 200
    raio = 0.3
    angulo = pi/2 -- 90°, sin(90°) = 1
    torque = calcularTorque forca raio angulo
    esperado = 200 * 0.3 * 1 -- 60 §N · m§
  in abs (torque - esperado) < 0.001

testeTorqueForcaParalela :: Bool
testeTorqueForcaParalela =
  let forca = 150
    raio = 0.4
    angulo = 0 -- 0°, sin(0°) = 0
    torque = calcularTorque forca raio angulo
  in abs torque < 0.001 -- deve ser zero

testeTorqueAngulo45Graus :: Bool
testeTorqueAngulo45Graus =
  let forca = 100
    raio = 1.0
    angulo = pi/4 -- 45°
    torque = calcularTorque forca raio angulo
    esperado = 100 * 1.0 * sin(pi/4) -- 100 * §√2/2 ≈ 70.71§
  in abs (torque - esperado) < 0.01

testeTorqueAngulo30Graus :: Bool
testeTorqueAngulo30Graus =
  let forca = 120
    raio = 0.8
    angulo = pi/6 -- 30°, sin(30°) = 0.5
    torque = calcularTorque forca raio angulo
    esperado = 120 * 0.8 * 0.5 -- 48 §N · m§
  in abs (torque - esperado) < 0.001

testeTorqueChaveInglesa :: Bool
testeTorqueChaveInglesa =
-- Caso real: chave inglesa de 25cm aplicando 80N
  let forca = 80
    raio = 0.25
    angulo = pi/2
    torque = calcularTorque forca raio angulo
    esperado = 20 -- 20 §N · m§
  in abs (torque - esperado) < 0.001

testeTorqueMotorEletrico :: Bool
testeTorqueMotorEletrico =
  -- Motor el´etrico: for¸ca tangencial no rotor
  let forca = 500
    raio = 0.15 -- raio do rotor
    angulo = pi/2
    torque = calcularTorque forca raio angulo
    esperado = 75 -- 75 §N · m§
  in abs (torque - esperado) < 0.001

testeTorqueValorGrande :: Bool
testeTorqueValorGrande =
  let forca = 10000 -- 10 kN
    raio = 2.0 -- 2 m
    angulo = pi/2
    torque = calcularTorque forca raio angulo
    esperado = 20000 -- 20 k§N · m§
  in abs (torque - esperado) < 1.0

testeTorqueValorPequeno :: Bool
testeTorqueValorPequeno =
  let forca = 0.1 -- 100 mN
    raio = 0.001 -- 1 mm
    angulo = pi/2
    torque = calcularTorque forca raio angulo
    esperado = 0.0001 -- 0.1 m§N · m§
  in abs (torque - esperado) < 0.00001

testeTorquePrecisao :: Bool
testeTorquePrecisao =
  let forca = 123.45
    raio = 0.6789
    angulo = 1.2345 -- ^angulo em radianos
    torque = calcularTorque forca raio angulo
    esperado = 123.45 * 0.6789 * sin(1.2345)
  in abs (torque - esperado) < 0.01

-- =================================
-- TESTES PARA VELOCIDADE ANGULAR
-- =================================
testeVelocidadeAngularBasica :: Bool
testeVelocidadeAngularBasica =
  let velocidade = 10 -- 10 m/s
    raio = 2 -- 2 m
    omega = velocidadeAngular velocidade raio
    esperada = 5 -- 5 rad/s
  in abs (omega - esperada) < 0.001

testeVelocidadeAngularRoda :: Bool
testeVelocidadeAngularRoda =
-- Roda de carro: v = 60 km/h = 16.67 m/s, raio = 0.3 m
  let velocidade = 16.67 -- ~60 km/h  
    raio = 0.3
    omega = velocidadeAngular velocidade raio
    esperada = 16.67 / 0.3 -- ~55.57 rad/s
  in abs (omega - esperada) < 0.1

testeVelocidadeAngularEngrenagem :: Bool
testeVelocidadeAngularEngrenagem =
  let velocidade = 5 -- velocidade tangencial
    raio = 0.05 -- engrenagem pequena, raio 5 cm
    omega = velocidadeAngular velocidade raio
    esperada = 100 -- 100 rad/s
  in abs (omega - esperada) < 0.001

testeVelocidadeAngularTurbina :: Bool
testeVelocidadeAngularTurbina =
  -- Turbina de vento
  let velocidade = 80 -- ponta da p´a a 80 m/s
    raio = 40 -- raio da turbina 40 m
    omega = velocidadeAngular velocidade raio
    esperada = 2 -- 2 rad/s
  in abs (omega - esperada) < 0.001

testeVelocidadeAngularZero :: Bool
testeVelocidadeAngularZero =
  let velocidade = 0 -- parado
    raio = 1
    omega = velocidadeAngular velocidade raio
  in abs omega < 0.001

testeVelocidadeAngularVelocidadeAlta :: Bool
testeVelocidadeAngularVelocidadeAlta =
  let velocidade = 1000 -- muito r´apido
    raio = 0.1
    omega = velocidadeAngular velocidade raio
    esperada = 10000 -- 10000 rad/s
  in abs (omega - esperada) < 1.0

testeVelocidadeAngularRaioGrande :: Bool
testeVelocidadeAngularRaioGrande =
  let velocidade = 30
    raio = 100 -- raio muito grande
    omega = velocidadeAngular velocidade raio
    esperada = 0.3 -- 0.3 rad/s
  in abs (omega - esperada) < 0.001

testeVelocidadeAngularRaioPequeno :: Bool
testeVelocidadeAngularRaioPequeno =
  let velocidade = 5
    raio = 0.001 -- 1 mm
    omega = velocidadeAngular velocidade raio
    esperada = 5000 -- 5000 rad/s
  in abs (omega - esperada) < 1.0

testeVelocidadeAngularDecimal :: Bool
testeVelocidadeAngularDecimal =
  let velocidade = 12.345
    raio = 2.468
    omega = velocidadeAngular velocidade raio
    esperada = 12.345 / 2.468
  in abs (omega - esperada) < 0.001

testeVelocidadeAngularRPM :: Bool
testeVelocidadeAngularRPM =
  -- Convers~ao para RPM: §ω§ = 100 rad/s = 100 * 60/(2§π§) RPM §≈§ 955 RPM
  let velocidade = 31.416 -- §π§ * 10
    raio = 0.1
    omega = velocidadeAngular velocidade raio
    esperada = 314.16 -- §π§ * 100 rad/s
  in abs (omega - esperada) < 0.1

-- =================================
-- TESTES PARA ACELERAC¸AO CENTR ~ IPETA ´
-- =================================

testeAceleracaocentripetaBasica :: Bool
testeAceleracaocentripetaBasica =
  let velocidade = 20 -- 20 m/s
    raio = 50 -- 50 m
    aceleracao = aceleracaocentripeta velocidade raio
    esperada = 400/50 -- 8 m/s²
  in abs (aceleracao - esperada) < 0.001

testeAceleracaocentripetaCarro :: Bool
testeAceleracaocentripetaCarro =
  -- Carro fazendo curva: 60 km/h (16.67 m/s) em curva de raio 80m
  let velocidade = 16.67
    raio = 80
    aceleracao = aceleracaocentripeta velocidade raio
    esperada = 16.67^2 / 80 -- ~3.47 m/s²
  in abs (aceleracao - esperada) < 0.1

testeAceleracaocentripetaAviao :: Bool
testeAceleracaocentripetaAviao =
  -- Avi~ao em manobra: 250 m/s, raio 1000m
  let velocidade = 250
    raio = 1000
    aceleracao = aceleracaocentripeta velocidade raio
    esperada = 62.5 -- 62.5 m/s² (~6.4g)
  in abs (aceleracao - esperada) < 0.1
  
testeAceleracaocentripetaSatelite :: Bool
testeAceleracaocentripetaSatelite =
  -- Sat´elite em ´orbita baixa: ~7800 m/s, raio ~6.67e6 m
  let velocidade = 7800
    raio = 6670000 -- ~raio da Terra + altitude
    aceleracao = aceleracaocentripeta velocidade raio
    esperada = 7800^2 / 6670000 -- ~9.1 m/s² (gravidade!)
  in abs (aceleracao - esperada) < 0.5

testeAceleracaocentripetaCentrifuga :: Bool
testeAceleracaocentripetaCentrifuga =
  -- Centr´ıfuga de laborat´orio: alta rota¸c~ao
  let velocidade = 100 -- 100 m/s na borda
    raio = 0.2 -- raio 20 cm
    aceleracao = aceleracaocentripeta velocidade raio
    esperada = 50000 -- 50000 m/s² (~5000g!)
  in abs (aceleracao - esperada) < 100

testeAceleracaocentripetaZeroVelocidade :: Bool
testeAceleracaocentripetaZeroVelocidade =
  let velocidade = 0
    raio = 10
    aceleracao = aceleracaocentripeta velocidade raio
  in abs aceleracao < 0.001

testeAceleracaocentripetaVelocidadeBaixa :: Bool
testeAceleracaocentripetaVelocidadeBaixa =
  let velocidade = 1 -- 1 m/s (pedestres)
    raio = 10
    aceleracao = aceleracaocentripeta velocidade raio
    esperada = 0.1 -- 0.1 m/s²
  in abs (aceleracao - esperada) < 0.001

testeAceleracaocentripetaRaioGrande :: Bool
testeAceleracaocentripetaRaioGrande =
  let velocidade = 30
    raio = 10000 -- curva muito suave
    aceleracao = aceleracaocentripeta velocidade raio
    esperada = 0.09 -- 0.09 m/s²
  in abs (aceleracao - esperada) < 0.001
  
testeAceleracaocentripetaRaioPequeno :: Bool
testeAceleracaocentripetaRaioPequeno =
  let velocidade = 5
    raio = 0.1 -- curva muito fechada
    aceleracao = aceleracaocentripeta velocidade raio
    esperada = 250 -- 250 m/s²
  in abs (aceleracao - esperada) < 0.1
  
testeAceleracaocentripetaPrecisao :: Bool
testeAceleracaocentripetaPrecisao =
  let velocidade = 15.678
    raio = 23.456
    aceleracao = aceleracaocentripeta velocidade raio
    esperada = 15.678^2 / 23.456
  in abs (aceleracao - esperada) < 0.001

-- =================================
-- TESTES PARA ENERGIA CINETICA ´
-- =================================

testeEnergiaCineticaBasica :: Bool
testeEnergiaCineticaBasica =
  let massa = 10 -- 10 kg
    velocidade = 20 -- 20 m/s
    energia = energiaCinetica massa velocidade
    esperada = 0.5 * 10 * 400 -- 2000 J
  in abs (energia - esperada) < 0.001

testeEnergiaCineticaCarro :: Bool
testeEnergiaCineticaCarro =
  -- Carro de 1200 kg a 60 km/h (16.67 m/s)
  let massa = 1200
    velocidade = 16.67
    energia = energiaCinetica massa velocidade
    esperada = 0.5 * 1200 * 16.67^2 -- ~166.7 kJ
  in abs (energia - esperada) < 1000

testeEnergiaCineticaBala :: Bool
testeEnergiaCineticaBala =
-- Bala de rifle: 10g a 800 m/s
  let massa = 0.01 -- 10 g
    velocidade = 800
    energia = energiaCinetica massa velocidade
    esperada = 0.5 * 0.01 * 800^2 -- 3200 J
  in abs (energia - esperada) < 1.0

testeEnergiaCineticaTrem :: Bool
testeEnergiaCineticaTrem =
  -- Trem: 500 toneladas a 100 km/h (27.78 m/s)
  let massa = 500000 -- 500 toneladas
    velocidade = 27.78
    energia = energiaCinetica massa velocidade
    esperada = 0.5 * 500000 * 27.78^2 -- ~193 MJ
  in abs (energia - esperada) < 100000

testeEnergiaCineticaPessoa :: Bool
testeEnergiaCineticaPessoa =
-- Pessoa correndo: 70 kg a 5 m/s
  let massa = 70
    velocidade = 5
    energia = energiaCinetica massa velocidade
    esperada = 0.5 * 70 * 25 -- 875 J
  in abs (energia - esperada) < 0.1

testeEnergiaCineticaVelocidadeZero :: Bool
testeEnergiaCineticaVelocidadeZero =
  let massa = 100
    velocidade = 0 -- parado
    energia = energiaCinetica massa velocidade
  in abs energia < 0.001

testeEnergiaCineticaMassaZero :: Bool
testeEnergiaCineticaMassaZero =
  let massa = 0 -- sem massa
    velocidade = 100
    energia = energiaCinetica massa velocidade
  in abs energia < 0.001

testeEnergiaCineticaVelocidadeAlta :: Bool
testeEnergiaCineticaVelocidadeAlta =
  -- Objeto a velocidades relativ´ısticas (ainda usando mec^anica cl´assica)
  let massa = 1
    velocidade = 100000 -- 100 km/s
    energia = energiaCinetica massa velocidade
    esperada = 0.5 * 1 * 100000^2 -- 5e9 J = 5 GJ
  in abs (energia - esperada) < 1e6

testeEnergiaCineticaMassaGrande :: Bool
testeEnergiaCineticaMassaGrande =
  -- Navio: 100.000 toneladas a 10 m/s
  let massa = 100000000 -- 100.000 toneladas
    velocidade = 10
    energia = energiaCinetica massa velocidade
    esperada = 0.5 * 100000000 * 100 -- 5e9 J
  in abs (energia - esperada) < 1e6

testeEnergiaCineticaDecimal :: Bool
testeEnergiaCineticaDecimal =
  let massa = 12.345
    velocidade = 6.789
    energia = energiaCinetica massa velocidade
    esperada = 0.5 * 12.345 * 6.789^2
  in abs (energia - esperada) < 0.01

-- =================================
-- TESTES PARA ENERGIA POTENCIAL
-- =================================

testeEnergiaPotencialBasica :: Bool
testeEnergiaPotencialBasica =
  let massa = 5 -- 5 kg
    altura = 10 -- 10 m
    energia = energiaPotencial massa altura
    esperada = 5 * 9.81 * 10 -- 490.5 J
  in abs (energia - esperada) < 0.1

testeEnergiaPotencialPessoa :: Bool
testeEnergiaPotencialPessoa =
  -- Pessoa de 70kg subindo escada de 3m
  let massa = 70
    altura = 3
    energia = energiaPotencial massa altura
    esperada = 70 * 9.81 * 3 -- ~2060 J
  in abs (energia - esperada) < 1.0
  
testeEnergiaPotencialElevador :: Bool
testeEnergiaPotencialElevador =
  -- Elevador com 8 pessoas (560 kg) subindo 50 m
  let massa = 560
    altura = 50
    energia = energiaPotencial massa altura
    esperada = 560 * 9.81 * 50 -- ~275 kJ
  in abs (energia - esperada) < 100

testeEnergiaPotencialHidroeletrica :: Bool
testeEnergiaPotencialHidroeletrica =
  -- Represa: 1 tonelada de ´agua a 100m de altura
  let massa = 1000 -- 1 tonelada
    altura = 100
    energia = energiaPotencial massa altura
    esperada = 1000 * 9.81 * 100 -- 981 kJ
  in abs (energia - esperada) < 100

testeEnergiaPotencialMontanha :: Bool
testeEnergiaPotencialMontanha =
  -- Escalador de 80kg no topo de montanha de 3000m
  let massa = 80
    altura = 3000
    energia = energiaPotencial massa altura
    esperada = 80 * 9.81 * 3000 -- ~2.35 MJ
  in abs (energia - esperada) < 1000

testeEnergiaPotencialAlturaZero :: Bool
testeEnergiaPotencialAlturaZero =
  let massa = 100
    altura = 0 -- no n´ıvel de refer^encia
    energia = energiaPotencial massa altura
  in abs energia < 0.001

testeEnergiaPotencialMassaZero :: Bool
testeEnergiaPotencialMassaZero =
  let massa = 0 -- sem massa
    altura = 100
    energia = energiaPotencial massa altura
  in abs energia < 0.001

testeEnergiaPotencialAlturaGrande :: Bool
testeEnergiaPotencialAlturaGrande =
  -- Sat´elite a 400 km de altitude
  let massa = 1000 -- 1 tonelada
    altura = 400000 -- 400 km
    energia = energiaPotencial massa altura
    esperada = 1000 * 9.81 * 400000 -- ~3.9 GJ
  in abs (energia - esperada) < 1e6

testeEnergiaPotencialDecimal :: Bool
testeEnergiaPotencialDecimal =
  let massa = 12.345
    altura = 67.89
    energia = energiaPotencial massa altura
    esperada = 12.345 * 9.81 * 67.89
  in abs (energia - esperada) < 1.0

testeEnergiaPotencialPrecisaoGravidade :: Bool
testeEnergiaPotencialPrecisaoGravidade =
  -- Verificar se usa g = 9.81 m/s²
  let massa = 1
    altura = 1
    energia = energiaPotencial massa altura
    esperada = 9.81 -- exatamente g
  in abs (energia - esperada) < 0.01

-- =================================
-- TESTES PARA CENTRO DE MASSA
-- =================================

testeCentroMassaBasico :: Bool
testeCentroMassaBasico =
  let pontos = [(2, 1), (4, 3), (6, 5)] -- (massa, posi¸c~ao)
    centro = centroMassaX pontos
    -- Centro = (2*1 + 4*3 + 6*5) / (2+4+6) = 44/12 = 3.67
    esperado = 44/12
  in abs (centro - esperado) < 0.01

testeCentroMassaDoisPontos :: Bool
testeCentroMassaDoisPontos =
  let pontos = [(10, 0), (10, 10)] -- massas iguais
    centro = centroMassaX pontos
    esperado = 5 -- ponto m´edio
  in abs (centro - esperado) < 0.001

testeCentroMassaMassasIguais :: Bool
testeCentroMassaMassasIguais =
  let pontos = [(1, -2), (1, 0), (1, 2), (1, 4)] -- massas unit´arias
    centro = centroMassaX pontos
    esperado = (-2 + 0 + 2 + 4) / 4 -- m´edia aritm´etica = 1
  in abs (centro - esperado) < 0.001

testeCentroMassaMassasDiferentes :: Bool
testeCentroMassaMassasDiferentes =
  let pontos = [(1, 0), (3, 10)] -- massa 3x maior em x=10
    centro = centroMassaX pontos
    -- (1*0 + 3*10) / (1+3) = 30/4 = 7.5
    esperado = 7.5
  in abs (centro - esperado) < 0.001

testeCentroMassaBarra :: Bool
testeCentroMassaBarra =
  -- Barra uniforme discretizada em 3 pontos
  let pontos = [(1, 0), (1, 1), (1, 2)] -- massa uniforme
    centro = centroMassaX pontos
    esperado = 1 -- centro geom´etrico
  in abs (centro - esperado) < 0.001

testeCentroMassaCorpoL :: Bool
testeCentroMassaCorpoL =
  -- Corpo em L: duas barras perpendiculares
  let pontos = [(2, 0.5), (2, 2.5)] -- centroides das barras
    centro = centroMassaX pontos
    esperado = 1.5 -- centro entre as barras
  in abs (centro - esperado) < 0.001

testeCentroMassaVeiculo :: Bool
testeCentroMassaVeiculo =
  -- Ve´ıculo: motor (300kg) em x=1m, carroceria (700kg) em x=3m
  let pontos = [(300, 1), (700, 3)]
    centro = centroMassaX pontos
    -- (300*1 + 700*3) / 1000 = 2400/1000 = 2.4
    esperado = 2.4
  in abs (centro - esperado) < 0.01

testeCentroMassaAviao :: Bool
testeCentroMassaAviao =
  -- Avi~ao: fuselagem (5000kg) em x=10m, asas (2000kg) em x=8m, cauda (500kg) em x=20
  let pontos = [(5000, 10), (2000, 8), (500, 20)]
    centro = centroMassaX pontos
    -- (5000*10 + 2000*8 + 500*20) / 7500 = 76000/7500 = 10.13
    esperado = 76000/7500
  in abs (centro - esperado) < 0.1

testeCentroMassaUmPonto :: Bool
testeCentroMassaUmPonto =
  let pontos = [(100, 5.5)] -- s´o um ponto
    centro = centroMassaX pontos
    esperado = 5.5 -- deve retornar a pr´opria posi¸c~ao
  in abs (centro - esperado) < 0.001

testeCentroMassaPosicoesNegativas :: Bool
testeCentroMassaPosicoesNegativas =
  let pontos = [(2, -5), (3, -1), (5, 3)]
    centro = centroMassaX pontos
    -- (2*(-5) + 3*(-1) + 5*3) / 10 = (-10-3+15) / 10 = 0.2
    esperado = 0.2
  in abs (centro - esperado) < 0.001

testeCentroMassaDecimal :: Bool
testeCentroMassaDecimal =
  let pontos = [(1.5, 2.3), (2.7, 4.8), (3.2, 1.9)]
    centro = centroMassaX pontos
    numerador = 1.5*2.3 + 2.7*4.8 + 3.2*1.9
    denominador = 1.5 + 2.7 + 3.2
    esperado = numerador / denominador
  in abs (centro - esperado) < 0.01

testeCentroMassaGrande :: Bool
testeCentroMassaGrande =
  -- Muitos pontos
  let pontos = [(fromIntegral i, fromIntegral (i*2)) | i <- [1..10]]
    centro = centroMassaX pontos
    -- §
    P§(i * 2i) / §
    P§(i) = §
    P§(2i²) / §
    P§(i) = 2*§
    P§(i²) / §
    P§(i)
    -- Para i=1..10: §
    P§(i) = 55, §
    P§(i²) = 385
    esperado = 2 * 385 / 55 -- = 14
  in abs (centro - esperado) < 0.1

-- =================================
-- TESTES INTEGRADOS E CASOS REAIS
-- =================================

testeCasoRealMotocicleta :: Bool
testeCasoRealMotocicleta =
  -- Motocicleta fazendo curva
  let massa = 250 -- 250 kg (moto + piloto)
    velocidade = 15 -- 15 m/s (54 km/h)
    raio = 30 -- curva de raio 30m
    -- C´alculos
    energiaCin = energiaCinetica massa velocidade
    aceleracao = aceleracaocentripeta velocidade raio
    omega = velocidadeAngular velocidade raio
    -- Verifica¸c~oes f´ısicas
    energiaOk = energiaCin > 25000 && energiaCin < 30000 -- ~28 kJ
    aceleracaoOk = aceleracao > 7 && aceleracao < 8 -- ~7.5 m/s²
    omegaOk = omega > 0.4 && omega < 0.6 -- ~0.5 rad/s
  in energiaOk && aceleracaoOk && omegaOk

testeCasoRealGuindaste :: Bool
testeCasoRealGuindaste =
  -- Guindaste levantando carga
  let massa = 2000 -- 2 toneladas
    altura = 20 -- 20 m
    raioLanca = 15 -- bra¸co do guindaste 15 m
    forcaLanca = 150000 -- for¸ca na lan¸ca
    -- C´alculos
    energiaPot = energiaPotencial massa altura
    torque = calcularTorque forcaLanca raioLanca (pi/2)
    -- Verifica¸c~oes
    energiaOk = energiaPot > 390000 && energiaPot < 400000 -- ~392 kJ
    torqueOk = torque > 2000000 && torque < 2500000 -- ~2.25 M§N · m§
  in energiaOk && torqueOk

testeCasoRealTurbina :: Bool
testeCasoRealTurbina =
  -- Turbina e´olica
  let raio = 50 -- raio das p´as 50 m
    velocidadeVento = 15 -- vento a 15 m/s
    velocidadePonta = velocidadeVento * 3 -- ponta 3x mais r´apida
    -- C´alculos
    omega = velocidadeAngular velocidadePonta raio
    aceleracao = aceleracaocentripeta velocidadePonta raio
    -- Verifica¸c~oes
    omegaOk = omega > 0.8 && omega < 1.0 -- ~0.9 rad/s
    aceleracaoOk = aceleracao > 40 && aceleracao < 45 -- ~40.5 m/s²
  in omegaOk && aceleracaoOk

testeCasoRealBalanca :: Bool
testeCasoRealBalanca =
  -- Sistema de alavanca/balan¸ca
  let pontos = [(5, -1), (10, 0), (15, 2)] -- massas na balan¸ca
    centro = centroMassaX pontos
    -- Para equil´ıbrio, momento total deve ser zero em rela¸c~ao ao centro
    momentoTotal = sum [(m * (x - centro)) | (m, x) <- pontos]
    -- Verifica¸c~ao: momento deve ser pr´oximo de zero (equil´ıbrio)
    equilibrioOk = abs momentoTotal < 0.1
    centroOk = centro > 0.8 && centro < 1.0
  in equilibrioOk && centroOk

testeCasoRealVooAviao :: Bool
testeCasoRealVooAviao =
  -- Avi~ao em voo nivelado fazendo curva
  let massa = 80000 -- 80 toneladas  
    velocidade = 250 -- 900 km/h = 250 m/s
    raio = 5000 -- curva suave de 5 km
    altitude = 10000 -- 10 km
    -- C´alculos
    energiaCin = energiaCinetica massa velocidade
    energiaPot = energiaPotencial massa altitude
    aceleracao = aceleracaocentripeta velocidade raio
    -- Verifica¸c~oes
    energiaCinOk = energiaCin > 2.4e9 && energiaCin < 2.6e9 -- ~2.5 GJ
    energiaPotOk = energiaPot > 7.8e9 && energiaPot < 7.9e9 -- ~7.86 GJ
    aceleracaoOk = aceleracao > 12 && aceleracao < 13 -- ~12.5 m/s²
  in energiaCinOk && energiaPotOk && aceleracaoOk

-- =================================
-- FUNC¸AO DE EXECUC¸ ~ AO DOS TESTES ~
-- =================================

executarTestesEngenhariaMecanica :: IO ()
executarTestesEngenhariaMecanica = do
    putStrLn "======================================"
    putStrLn " TESTES ENGENHARIA MECANICA"
    putStrLn "======================================"
    putStrLn "\n-- TESTES TORQUE --"
    putStrLn $ "B´asico: " ++ show testeTorqueBasico
    putStrLn $ "For¸ca perpendicular: " ++ show testeTorqueForcaPerpendicular
    putStrLn $ "For¸ca paralela: " ++ show testeTorqueForcaParalela
    putStrLn $ "^Angulo 45°: " ++ show testeTorqueAngulo45Graus
    putStrLn $ "^Angulo 30°: " ++ show testeTorqueAngulo30Graus
    putStrLn $ "Chave inglesa: " ++ show testeTorqueChaveInglesa
    putStrLn $ "Motor el´etrico: " ++ show testeTorqueMotorEletrico
    putStrLn $ "Valor grande: " ++ show testeTorqueValorGrande
    putStrLn $ "Valor pequeno: " ++ show testeTorqueValorPequeno
    putStrLn $ "Precis~ao: " ++ show testeTorquePrecisao
    putStrLn "\n-- TESTES VELOCIDADE ANGULAR --"
    putStrLn $ "B´asica: " ++ show testeVelocidadeAngularBasica
    putStrLn $ "Roda: " ++ show testeVelocidadeAngularRoda
    putStrLn $ "Engrenagem: " ++ show testeVelocidadeAngularEngrenagem
    putStrLn $ "Turbina: " ++ show testeVelocidadeAngularTurbina
    putStrLn $ "Zero: " ++ show testeVelocidadeAngularZero
    putStrLn $ "Velocidade alta: " ++ show testeVelocidadeAngularVelocidadeAlta
    putStrLn $ "Raio grande: " ++ show testeVelocidadeAngularRaioGrande
    putStrLn $ "Raio pequeno: " ++ show testeVelocidadeAngularRaioPequeno
    putStrLn $ "Decimal: " ++ show testeVelocidadeAngularDecimal
    putStrLn $ "RPM: " ++ show testeVelocidadeAngularRPM
    putStrLn "\n-- TESTES ACELERAC¸~AO CENTR´IPETA --"
    putStrLn $ "B´asica: " ++ show testeAceleracaocentripetaBasica
    putStrLn $ "Carro: " ++ show testeAceleracaocentripetaCarro
    putStrLn $ "Avi~ao: " ++ show testeAceleracaocentripetaAviao
    putStrLn $ "Sat´elite: " ++ show testeAceleracaocentripetaSatelite
    putStrLn $ "Centr´ıfuga: " ++ show testeAceleracaocentripetaCentrifuga
    putStrLn $ "Zero velocidade: " ++ show testeAceleracaocentripetaZeroVelocidade
    putStrLn $ "Velocidade baixa: " ++ show testeAceleracaocentripetaVelocidadeBaixa
    putStrLn $ "Raio grande: " ++ show testeAceleracaocentripetaRaioGrande
    putStrLn $ "Raio pequeno: " ++ show testeAceleracaocentripetaRaioPequeno
    putStrLn $ "Precis~ao: " ++ show testeAceleracaocentripetaPrecisao
    putStrLn "\n-- TESTES ENERGIA CIN´ETICA --"
    putStrLn $ "B´asica: " ++ show testeEnergiaCineticaBasica
    putStrLn $ "Carro: " ++ show testeEnergiaCineticaCarro
    putStrLn $ "Bala: " ++ show testeEnergiaCineticaBala
    putStrLn $ "Trem: " ++ show testeEnergiaCineticaTrem
    putStrLn $ "Pessoa: " ++ show testeEnergiaCineticaPessoa
    putStrLn $ "Velocidade zero: " ++ show testeEnergiaCineticaVelocidadeZero
    putStrLn $ "Massa zero: " ++ show testeEnergiaCineticaMassaZero
    putStrLn $ "Velocidade alta: " ++ show testeEnergiaCineticaVelocidadeAlta
    putStrLn $ "Massa grande: " ++ show testeEnergiaCineticaMassaGrande
    putStrLn $ "Decimal: " ++ show testeEnergiaCineticaDecimal
    putStrLn "\n-- TESTES ENERGIA POTENCIAL --"
    putStrLn $ "B´asica: " ++ show testeEnergiaPotencialBasica
    putStrLn $ "Pessoa: " ++ show testeEnergiaPotencialPessoa
    putStrLn $ "Elevador: " ++ show testeEnergiaPotencialElevador
    putStrLn $ "Hidrel´etrica: " ++ show testeEnergiaPotencialHidroeletrica
    putStrLn $ "Montanha: " ++ show testeEnergiaPotencialMontanha
    putStrLn $ "Altura zero: " ++ show testeEnergiaPotencialAlturaZero
    putStrLn $ "Massa zero: " ++ show testeEnergiaPotencialMassaZero
    putStrLn $ "Altura grande: " ++ show testeEnergiaPotencialAlturaGrande
    putStrLn $ "Decimal: " ++ show testeEnergiaPotencialDecimal
    putStrLn $ "Precis~ao gravidade: " ++ show testeEnergiaPotencialPrecisaoGravidade
    putStrLn "\n-- TESTES CENTRO DE MASSA --"
    putStrLn $ "B´asico: " ++ show testeCentroMassaBasico
    putStrLn $ "Dois pontos: " ++ show testeCentroMassaDoisPontos
    putStrLn $ "Massas iguais: " ++ show testeCentroMassaMassasIguais
    putStrLn $ "Massas diferentes: " ++ show testeCentroMassaMassasDiferentes
    putStrLn $ "Barra: " ++ show testeCentroMassaBarra
    putStrLn $ "Corpo L: " ++ show testeCentroMassaCorpoL
    putStrLn $ "Ve´ıculo: " ++ show testeCentroMassaVeiculo
    putStrLn $ "Avi~ao: " ++ show testeCentroMassaAviao
    putStrLn $ "Um ponto: " ++ show testeCentroMassaUmPonto
    putStrLn $ "Posi¸c~oes negativas: " ++ show testeCentroMassaPosicoesNegativas
    putStrLn $ "Decimal: " ++ show testeCentroMassaDecimal
    putStrLn $ "Grande: " ++ show testeCentroMassaGrande
    putStrLn "\n-- TESTES CASOS REAIS --"
    putStrLn $ "Motocicleta: " ++ show testeCasoRealMotocicleta
    putStrLn $ "Guindaste: " ++ show testeCasoRealGuindaste
    putStrLn $ "Turbina: " ++ show testeCasoRealTurbina
    putStrLn $ "Balan¸ca: " ++ show testeCasoRealBalanca
    putStrLn $ "Voo avi~ao: " ++ show testeCasoRealVooAviao

testesEngenhariaMecanica :: [(String, Bool)]
testesEngenhariaMecanica =
  [ -- Torque (10 testes)
    ("Torque B´asico", testeTorqueBasico)
    , ("Torque For¸ca Perpendicular", testeTorqueForcaPerpendicular)
    , ("Torque For¸ca Paralela", testeTorqueForcaParalela)
    , ("Torque ^Angulo 45°", testeTorqueAngulo45Graus)
    , ("Torque ^Angulo 30°", testeTorqueAngulo30Graus)
    , ("Torque Chave Inglesa", testeTorqueChaveInglesa)
    , ("Torque Motor El´etrico", testeTorqueMotorEletrico)
    , ("Torque Valor Grande", testeTorqueValorGrande)
    , ("Torque Valor Pequeno", testeTorqueValorPequeno)
    , ("Torque Precis~ao", testeTorquePrecisao)
    -- Velocidade Angular (10 testes)
    , ("Velocidade Angular B´asica", testeVelocidadeAngularBasica)
    , ("Velocidade Angular Roda", testeVelocidadeAngularRoda)
    , ("Velocidade Angular Engrenagem", testeVelocidadeAngularEngrenagem)
    , ("Velocidade Angular Turbina", testeVelocidadeAngularTurbina)
    , ("Velocidade Angular Zero", testeVelocidadeAngularZero)
    , ("Velocidade Angular Alta", testeVelocidadeAngularVelocidadeAlta)
    , ("Velocidade Angular Raio Grande", testeVelocidadeAngularRaioGrande)
    , ("Velocidade Angular Raio Pequeno", testeVelocidadeAngularRaioPequeno)
    , ("Velocidade Angular Decimal", testeVelocidadeAngularDecimal)
    , ("Velocidade Angular RPM", testeVelocidadeAngularRPM)
    -- Acelera¸c~ao Centr´ıpeta (10 testes)
    , ("Acelera¸c~ao Centr´ıpeta B´asica", testeAceleracaocentripetaBasica)
    , ("Acelera¸c~ao Centr´ıpeta Carro", testeAceleracaocentripetaCarro)
    , ("Acelera¸c~ao Centr´ıpeta Avi~ao", testeAceleracaocentripetaAviao)
    , ("Acelera¸c~ao Centr´ıpeta Sat´elite", testeAceleracaocentripetaSatelite)
    , ("Acelera¸c~ao Centr´ıpeta Centr´ıfuga", testeAceleracaocentripetaCentrifuga)
    , ("Acelera¸c~ao Centr´ıpeta Zero", testeAceleracaocentripetaZeroVelocidade)
    , ("Acelera¸c~ao Centr´ıpeta Baixa", testeAceleracaocentripetaVelocidadeBaixa)
    , ("Acelera¸c~ao Centr´ıpeta Raio Grande", testeAceleracaocentripetaRaioGrande)
    , ("Acelera¸c~ao Centr´ıpeta Raio Pequeno", testeAceleracaocentripetaRaioPequeno)
    , ("Acelera¸c~ao Centr´ıpeta Precis~ao", testeAceleracaocentripetaPrecisao)
    -- Energia Cin´etica (10 testes)
    , ("Energia Cin´etica B´asica", testeEnergiaCineticaBasica)
    , ("Energia Cin´etica Carro", testeEnergiaCineticaCarro)
    , ("Energia Cin´etica Bala", testeEnergiaCineticaBala)
    , ("Energia Cin´etica Trem", testeEnergiaCineticaTrem)
    , ("Energia Cin´etica Pessoa", testeEnergiaCineticaPessoa)
    , ("Energia Cin´etica Velocidade Zero", testeEnergiaCineticaVelocidadeZero)
    , ("Energia Cin´etica Massa Zero", testeEnergiaCineticaMassaZero)
    , ("Energia Cin´etica Velocidade Alta", testeEnergiaCineticaVelocidadeAlta)
    , ("Energia Cin´etica Massa Grande", testeEnergiaCineticaMassaGrande)
    , ("Energia Cin´etica Decimal", testeEnergiaCineticaDecimal)
    -- Energia Potencial (10 testes)
    , ("Energia Potencial B´asica", testeEnergiaPotencialBasica)
    , ("Energia Potencial Pessoa", testeEnergiaPotencialPessoa)
    , ("Energia Potencial Elevador", testeEnergiaPotencialElevador)
    , ("Energia Potencial Hidrel´etrica", testeEnergiaPotencialHidroeletrica)
    , ("Energia Potencial Montanha", testeEnergiaPotencialMontanha)
    , ("Energia Potencial Altura Zero", testeEnergiaPotencialAlturaZero)
    , ("Energia Potencial Massa Zero", testeEnergiaPotencialMassaZero)
    , ("Energia Potencial Altura Grande", testeEnergiaPotencialAlturaGrande)
    , ("Energia Potencial Decimal", testeEnergiaPotencialDecimal)
    , ("Energia Potencial Gravidade", testeEnergiaPotencialPrecisaoGravidade)
    -- Centro de Massa (12 testes)
    , ("Centro Massa B´asico", testeCentroMassaBasico)
    , ("Centro Massa Dois Pontos", testeCentroMassaDoisPontos)
    , ("Centro Massa Massas Iguais", testeCentroMassaMassasIguais)
    , ("Centro Massa Massas Diferentes", testeCentroMassaMassasDiferentes)
    , ("Centro Massa Barra", testeCentroMassaBarra)
    , ("Centro Massa Corpo L", testeCentroMassaCorpoL)
    , ("Centro Massa Ve´ıculo", testeCentroMassaVeiculo)
    , ("Centro Massa Avi~ao", testeCentroMassaAviao)
    , ("Centro Massa Um Ponto", testeCentroMassaUmPonto)
    , ("Centro Massa Posi¸c~oes Negativas", testeCentroMassaPosicoesNegativas)
    , ("Centro Massa Decimal", testeCentroMassaDecimal)
    , ("Centro Massa Grande", testeCentroMassaGrande)
    -- Casos Reais (5 testes)
    , ("Caso Real Motocicleta", testeCasoRealMotocicleta)
    , ("Caso Real Guindaste", testeCasoRealGuindaste)
    , ("Caso Real Turbina", testeCasoRealTurbina)
    , ("Caso Real Balan¸ca", testeCasoRealBalanca)
    , ("Caso Real Voo Avi~ao", testeCasoRealVooAvi~ao)
    ]
