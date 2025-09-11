module Engineering.ALL where

import Types

momentoInerciaRetangular :: Largura -> Altura -> MomentoInercia
momentoInerciaRetangular largura_base altura_seccao =
   (largura_base * (altura_seccao ^ 3)) / 12
   
tensaoNormal :: Forca -> Area -> Pressao
tensaoNormal _ 0 = error  "Erro: Área da secção não pode ser zero, acarretando em divisão por zero, gerando indeterminação"
tensaoNormal forca_aplicada a_seccao_transversal =
   forca_aplicada / a_seccao_transversal

deflexaoViga :: Forca-> Comprimento-> ModuloElasticidade-> MomentoInercia-> Distancia
deflexaoViga _ _ 0 _ = error "Erro: Módulo da elasticidade não pode ser zero, acarretando em divisão por zero, gerando indeterminação"
deflexaoViga _ _ _ 0 = error "Erro: Momento da inércia não pode ser zero, acarretando em divisão por zero, gerando indeterminação"
deflexaoViga carga comprimento elasticidade inercia = 
        (carga * comprimento ** 3 ) / ( 48 * elasticidade * inercia )

cargaCriticaEuler :: ModuloElasticidade-> MomentoInercia-> Comprimento-> Forca
cargaCriticaEuler _ _ 0 = error "Erro: Módulo do comprimento não pode ser zero, acarretando em divisão por zero, gerando indeterminação"
cargaCriticaEuler elasticidade inercia comprimento = 
        ( pi ** 2 * elasticidade * inercia ) / ( comprimento ** 2 )
        
volumeConcreto :: Figura-> Volume
volumeConcreto (Retangulo _ _)         = error "Erro: Não existe volume de figuras planas"
volumeConcreto (Circulo _)             = error "Erro: Não existe volume de figuras planas"
volumeConcreto (Triangulo _ _ _)       = error "Erro: Não existe volume de figuras planas"
volumeConcreto (Poligono _)            = error "Erro: Não existe volume de figuras planas"
volumeConcreto (Esfera raio)           = (4/3) * pi * (raio **3 )
volumeConcreto (Cilindro raio altura ) = pi * (raio**2) * altura
volumeConcreto (Paralelepipedo comprimento largura altura ) = comprimento * largura * altura

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

calcularTorque :: Forca-> Distancia-> Angulo-> Torque
calcularTorque força distancia_eixo angulo_força_raio = 
        força * distancia_eixo * sin angulo_força_raio 

velocidadeAngular :: Velocidade-> Raio-> VelocidadeAngular
velocidadeAngular _ 0 = error "Erro: Módulo do raio não pode ser zero, acarretando em divisão por zero, gerando indeterminação"
velocidadeAngular velocidade_linear raio = 
        velocidade_linear / raio

aceleracaocentripeta :: Velocidade-> Raio-> Aceleracao
aceleracaocentripeta _ 0 = error "Erro: Módulo do raio não pode ser zero, acarretando em divisão por zero, gerando indeterminação"
aceleracaocentripeta velocidade_tangecial raio = 
        (velocidade_tangecial ** 2) / raio
        
energiaCinetica :: Massa-> Velocidade-> Energia
energiaCinetica massa velocidade = 0.5 * massa * ( velocidade ** 2 )

energiaPotencial :: Massa-> Altura-> Energia
energiaPotencial massa altura = massa * 9.81 * altura 

centroMassaX :: [(Massa, Distancia)]-> Distancia
centroMassaX [] = error "Erro: Não há como calcular o centro de Massa de uma lista sem elementos"
centroMassaX lista = soma_massa_posição lista / soma_massas lista
    where soma_massa_posição [ ] = 0
          soma_massa_posição ((a,b) : xs ) = a * b + soma_massa_posição xs
          
          soma_massas [ ] = 0
          soma_massas ((a,_) : xs ) = a + soma_massas xs 
