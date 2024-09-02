module Tipos where
import Data.Time (Day) 

data Cliente = Cliente {
    cpfCliente :: String,
    nomeCliente :: String,
    telefoneCliente :: String,
    sexoCliente :: String,
    idadeCliente :: Int,
    estadoCivilCliente :: String,
    nivelCliente :: Int, -- entre 1 e 5, o default é 2
    statusFinanceiroCliente :: Bool, -- True se estiver em dia com o pagamento
    tempoFidelidadeCliente :: Int, -- em meses
    numSinistrosCliente :: Int,
    veiculoCliente :: Veiculo
} deriving (Show, Eq)

data Veiculo = Veiculo {
    modeloVeiculo :: String,
    anoVeiculo :: Int,
    tipoVeiculo :: String,
    placaVeiculo :: String
} deriving (Show, Eq)

data Seguro = Seguro {
    clienteSeguro :: Cliente,  -- Aqui armazenamos o Cliente completo
    automovelSeguro :: Veiculo,
    tipoContratoSeguro :: String, -- básico, tradicional ou premium
    valorContratoSeguro :: Float,
    dataEmissaoSeguro :: Day
} deriving (Show, Eq)

data Sinistro = Sinistro {
    idSinistro :: String,
    cpfClienteSinistro :: String,
    idSeguroSinistro :: String,
    nivelAcidente :: String, -- leve, médio ou perca total
    custoSinistro :: Float,
    dataSinistro :: Day,
    ativoSinistro :: Bool
} deriving (Show, Eq)
