module Tipos where

data Cliente = Cliente {
    cpfCliente :: String,
    nomeCliente :: String,
    nivelClienteCliente :: Int, --varia entre 1 e 5, o default é 2
    sexoCliente :: String,
    idadeCliente :: Int,
    estadoCivilCliente :: String,
    nivelDeRiscoCliente :: Float,
    telefoneCliente :: String,
    listaSegurosCliente :: [Seguro], -- é pra ser uma lista, nao sei como implementar ainda
    statusFinanceiroCliente :: Bool, --true se tiver em dia com o pagamento
    tempoFidelidadeCliente :: Int, -- em meses
    numSinistrosCliente :: Int,
    nivelCliente :: Int,
    tipoAutomovelCliente :: String,
    veiculoCliente :: Veiculo
} deriving (Show, Eq)

data Veiculo = Veiculo {
    modeloVeiculo :: String,
    anoVeiculo :: String,
    tipoVeiculo :: String,
    placaVeiculo :: String
} deriving (Show, Eq)

data Seguro = Seguro {
    idSeguroSeguro :: String,
    cpfClienteSeguro :: String,
    automovelSeguro :: String,
    tipoAutomovelSeguro :: String,
    tipoContratoSeguro :: String --basico, tradicional ou premium
} deriving (Show, Eq)

data Sinistro = Sinistro {
    idSinistroSinistro :: String,
    cpfClienteSinistro :: String,
    idSeguroSinistro :: String,
    nivelAcidenteSinistro :: String, --leve, médio ou perca total
    custoSinistro :: Float, -- vai ser calculado com base em variaveis
    dataSinistro :: String
} deriving (Show, Eq)
