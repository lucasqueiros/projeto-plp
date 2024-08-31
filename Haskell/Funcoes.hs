import Data.List (intercalate)
module Funcoes where

-- vai pegar todos os atributos necessarios e retornar um inteiro de 1 a 5
calcularNivelCliente :: Int -> String -> Int --esse input Int e String eh um exemplo
calcularNivelCliente = 3

--vai receber o custo do sinistro e um inteiro do nivel do cliente e retorna o valor do desconto
calcularDesconto :: Float -> Int -> Float
calculaDesconto = 10.5

--limpar a tela
limparTela :: String
limparTela = ""

--simula o seguro baseado em atributos do cliente
simularSeguro :: String -> String -> String
simularSeguro = ""

-- Função que Verifica o Status Financeiro do Seguro
pagamentoEmDia :: Bool -> String 
pagamentoEmDia status = 
    if status
        then "Em dia!"
    else "Devendo!"

-- Função para formatar um Sinistro como uma String
formatarSinistro :: Sinistro -> String
formatarSinistro sinistro =
    intercalate " | " [idSinistro sinistro, nivelAcidente sinistro, show (custo sinistro), dataSinistro sinistro] -- troquei 'data' por 'dataSinistro' pq tava dando conflito


-- Função que gera um relatório com com informações acerca de um cliente registrado.
gerarRelatorio :: Cliente -> [Sinistro] -> String
gerarRelatorio cliente sinistros =
    let
        seguro = listaSeguros cliente
        -- Informações pessoais do cliente e de seu automóvel.
        infoCliente = "Nome: " ++ nome cliente ++ "\n" ++
                      "CPF: " ++ cpf cliente ++ "\n" ++
                      "Telefone: " ++ telefone cliente ++ "\n" ++
                      "Idade: " ++ show (idade cliente) ++ "\n" ++
                      "Sexo: " ++ sexo cliente ++ "\n" ++
                      "Estado Civíl: " ++ estadoCivil cliente ++ "\n"

        -- Detalhes do seguro do cliente.
        nivelcliente = nivelCliente cliente
        infoNivel = "Nível de Cliente: " ++ show (nivelCliente cliente) ++ "\n" ++
                    "Tipo de Contrato: " ++ tipoContrato seguro ++ "\n" ++
                    "Status Financeiro: " ++ pagamentoEmDia (statusFinanceiro cliente) ++ "\n" ++
                    "Tipo de automóvel: " ++ tipoAutomovel cliente ++ "\n"

        -- Detalhes do histórico de sinistros
        infoSinistros = "Sinistros Registrados: " ++ show (numSinistros cliente) ++ "\n" ++
                        "Detalhes dos Sinistros: \n" ++ intercalate "\n" (map formatarSinistro sinistros)
    in
        -- Combinando as duas partes do relatório.
        infoCliente ++ "\n" ++ infoNivel ++ "\n" ++ infoSinistros
