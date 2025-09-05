module Engineering.Electrical where

import Types

tensaoOhm :: Corrente-> Resistencia-> Tensao
tensaoOhm corrente resistencia = 
        corrente * resistencia

potenciaEletricaVI :: Tensao-> Corrente-> PotenciaEletrica
potenciaEletricaVI tensao corrente = 
        tensao * corrente

potenciaEletricaRI :: Resistencia-> Corrente-> PotenciaEletrica
potenciaEletricaRI resistencia corrente = 
        resistencia * ( corrente ** 2 )

potenciaEletricaVR :: Tensao-> Resistencia-> PotenciaEletrica
potenciaEletricaVR _ 0 = error "Erro: Módulo da resistência elétrica não pode ser zero, acarretando em divisão por zero, gerando indeterminação"
potenciaEletricaVR tensao resistencia = (
        tensao ** 2) / resistencia

resistenciaSerie :: [Resistencia]-> Resistencia
resistenciaSerie [ ] = error "Erro: Não há como calcular a resistencia em série de uma lista sem elementos"
resistenciaSerie lista = sum lista

resistenciaParalelo :: [Resistencia]-> Resistencia
resistenciaParalelo [ ] = error "Erro: Não há como calcular a resistencia em paralelo de uma lista sem elementos"
resistenciaParalelo lista  = 1 / sum ( map (1/) lista )

impedanciaAC :: Resistencia-> Reatancia-> Impedancia
impedanciaAC resistencia reatancia = 
        sqrt ( resistencia ** 2 + reatancia ** 2 )

polarParaRetangular :: Double-> Angulo-> (Double, Double)
polarParaRetangular coordenada_radial angulo_teta = 
        ( coordenada_radial * cos angulo_teta , coordenada_radial * sin angulo_teta )

retangularParaPolar :: Double-> Double-> (Double, Angulo)
retangularParaPolar x y = 
        ( sqrt ( x ** 2 + y ** 2 ), atan2 y x )
