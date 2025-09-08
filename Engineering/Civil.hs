module Engineering.Civil where

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

