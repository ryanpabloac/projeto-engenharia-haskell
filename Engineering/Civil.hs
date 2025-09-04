module Engineering.Civil where

import Types

momentoInerciaRetangular :: Largura -> Altura -> MomentoInercia
momentoInerciaRetangular largura_base altura_seccao =
   (largura_base * (altura_seccao ^ 3)) / 12
   
tensaoNormal :: Forca -> Area -> Pressao
tensaoNormal _ 0 = error "Área de seção não pode ser 0 (divisão por 0 - indeterminação)"
tensaoNormal forca_aplicada a_seccao_transversal =
   forca_aplicada / a_seccao_transversal
