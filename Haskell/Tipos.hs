module Tipos where

data Cliente = Cliente {
    cpf :: String
    nome :: String
    nivelCliente :: Int --varia entre 1 e 5, o default é 2
    sexo :: String
    idade :: Int
    estadoCivil :: String
    nivelDeRisco :: Float 
    telefone :: String
    listaSeguros :: String -- é pra ser uma lista, nao sei como implementar ainda
    statusFinanceiro :: Bool --true se tiver em dia com o pagamento
    numSinistros :: Int
    tipoAutomovel :: String
} deriving (Show, Eq)

data Seguro = Seguro {
    idSeguro :: String
    cpfCliente :: String
    automovel :: String
    tipoContrato :: String --basico, tradicional ou premium
} deriving (Show, Eq)

data Sinistro = Sinistro {
    idSinistro :: String
    cpfCliente :: String
    idSeguro :: String
    nivelAcidente :: String --leve, médio ou perca total
    custo :: Float -- vai ser calculado com base em variaveis
    data :: String
} deriving (Show, Eq)
