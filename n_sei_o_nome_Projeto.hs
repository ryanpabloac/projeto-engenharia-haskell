calcularCustoTotal :: Projeto -> Custo
calcularCustoTotal projeto =
    sum [ precoUnitario m * quantidade m | m <- materiais projeto ]
