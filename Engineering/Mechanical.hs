module Engineering.Mechanical where

import Types

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
