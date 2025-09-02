module Types where
    
-- Distâncias e comprimentos
type Distancia = Double -- metros(m)
type Comprimento = Double -- metros(m)
type Altura = Double -- metros(m)
type Largura = Double -- metros(m)
type Espessura = Double -- metros(m)
type Raio = Double -- metros(m)
type Diametro = Double -- metros(m)

-- Áreas e volumes
type Area = Double -- metros quadrados(m²)
type Volume = Double -- metros cúbicos(m³)
type Perimetro = Double -- metros(m)

-- Ângulos
type Angulo = Double -- radianos(rad)
type AnguloGraus = Double -- graus(º)

-- Forças e Momentos
type Forca = Double -- Newtons(N)
type Momento = Double -- Newton-metro (N§·§m)
type Torque = Double -- Newton-metro (N§·§m)
type Pressao = Double -- Pascal (Pa) ou (MPa)

-- Massas e densidades
type Massa = Double -- quilogramas (kg)
type Densidade = Double -- kg/m³
type Peso = Double -- Newtons(N)
 
-- Velocidades e acelerações
type Velocidade = Double -- metros por segundo(m/s)
type Aceleracao = Double -- metros por segundo² (m/s²)
type VelocidadeAngular = Double-- radianos por segundo (rad/s)

-- Energias
type Energia = Double -- Joules(J)
type Potencia = Double -- Watts(W)
type Trabalho = Double -- Joules(J)

-- Grandezas Básicas
type Tensao = Double -- Volts(V)
type Corrente = Double -- Ampères(A)
type Resistencia = Double -- Ohms(§Ω§)
type Capacitancia = Double-- Farads(F)
type Indutancia = Double-- Henrys(H)

-- Potências elétricas
type PotenciaEletrica = Double -- Watts(W)
type PotenciaReativa = Double -- VAR
type PotenciaAparente = Double -- VA

-- Impedâncias
type Impedancia = Double -- Ohms (§Ω§)
type Reatancia = Double -- Ohms (§Ω§)
type FatorPotencia = Double-- adimensional (0 a 1)

-- Propriedades mecânicas
type ModuloElasticidade = Double-- Pascal (Pa)
type ResistenciaTracao = Double-- Pascal (Pa)
type ResistenciaCompressao = Double-- Pascal (Pa)
type TensaoCisalhamento = Double-- Pascal (Pa)
type FatorSeguranca = Double -- adimensional

-- Propriedades térmicas
type Temperatura = Double-- Celsius (°C) ou Kelvin (K)
type CondutividadeTermica = Double-- W/(m§·§K)
type CalorEspecifico = Double-- J/(kg§·§K)

-- Propriedades geométricas
type MomentoInercia = Double -- §m^4§
type ModuloResistencia = Double -- m³
type RaioGiracao = Double-- metros(m)

-- Custos e valores
type Custo = Double -- Reais (R)
type Preco = Double -- Reais por unidade(R/unidade)
type Orcamento = Double -- Reais(R)
type CustoUnitario = Double -- Reais por unidade(R/unidade)

-- Tempos
type Tempo = Double -- segundos(s)
type Duracao = Double -- horas(h)
type Prazo = Int -- dias


-- Tipos Algébricos
data Status = Planejamento | EmDesenvolvimento | EmRevisao
            | Concluido | Cancelado
    deriving (Show, Eq, Ord)

data TipoProjeto = Civil | Mecanica | Eletrica | Estrutural
    deriving (Show, Eq)

data Ponto2D = Ponto2D Distancia Distancia deriving (Show, Eq)

data Ponto3D = Ponto3D Distancia Distancia Distancia deriving (Show, Eq)

data Figura = Retangulo Largura Altura
            | Circulo Raio
            | Triangulo Ponto2D Ponto2D Ponto2D
            | Poligono [Ponto2D]
            | Esfera Raio
            | Cilindro Raio Altura
            | Paralelepipedo Comprimento Largura Altura
    deriving (Show, Eq)

data Matriz = Matriz [[Double]] deriving (Show, Eq)

data Vetor = Vetor [Double] deriving (Show, Eq)

data TipoFuncao = Linear Double Double-- ax + b
                | Quadratica Double Double Double-- ax² + bx + c
                | Exponencial Double Double-- a * e^(bx)
                | Logaritmica Double Double-- a * ln(bx)
                | Trigonometrica TipoTrig Double Double-- a * func(bx)
     deriving (Show, Eq)

data TipoTrig = Seno | Cosseno | Tangente deriving (Show, Eq)

data Funcao = Funcao TipoFuncao String deriving (Show, Eq)-- Tipo e descrição

data Projeto = Projeto
    { idProjeto :: Int
    , nomeProjeto :: String
    , tipoProjeto :: TipoProjeto
    , status :: Status
    , coordenadas :: [Ponto3D] -- Pontos do projeto
    , figuras :: [Figura] -- Formas geométricas
    , materiais :: [Material] -- Materiais utilizados
    , calculos :: [CalculoMat] -- Cálculos realizados
    , funcoes :: [Funcao] -- Funções matemáticas
    , dataInicio :: Day
    , dataFim :: Maybe Day
    , orcamento :: Orcamento
    } deriving (Show, Eq)

data Material = Material
    { nomeMaterial :: String
    , densidade :: Densidade
    , resistencia :: ResistenciaTracao
    , custo :: CustoUnitario
    , quantidade :: Volume
    } deriving (Show, Eq)

data CalculoMat = CalculoMat
    { nomeCalculo :: String
    , entrada :: [Double]
    , resultado :: Double
    , unidade :: String
    , formula :: String
    } deriving (Show, Eq)
    
-- Tipo para cargas estruturais
data Carga = CargaPontual Forca Ponto2D
           | CargaDistribuida Forca Comprimento -- forçaa por metro
           | CargaTriangular Forca Forca Comprimento -- carga variável
    deriving (Show, Eq)

-- Tipo para propriedades de seções
data SecaoTransversal = SecaoRetangular Largura Altura
                      | SecaoCircular Raio
                      | SecaoI Altura Largura Espessura Espessura-- h, b, tw, tf
    deriving (Show, Eq)