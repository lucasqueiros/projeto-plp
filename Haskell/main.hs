module Main where

import Menu as Menu
import Funcoes
import Tipos
import Data.Char (isDigit, isAlpha)
import Data.Time
import Data.Maybe (isNothing, fromJust)

-- Função auxiliar para calcular a idade com base na data de nascimento
calcularIdade :: Day -> IO Int
calcularIdade dataNascimento = do
    dataAtual <- getCurrentTime
    let (anoAtual, mesAtual, diaAtual) = toGregorian (utctDay dataAtual)
        (anoNasc, mesNasc, diaNasc) = toGregorian dataNascimento
        idade = fromInteger (anoAtual - anoNasc) - if (mesAtual, diaAtual) < (mesNasc, diaNasc) then 1 else 0
    return idade

-- Função para validar se o CPF tem exatamente 11 dígitos
validarCpf :: String -> Bool
validarCpf cpf = length cpf == 11 && all isDigit cpf



-- Função para buscar se o CPF já está registrado
buscarCpfRegistrado :: String -> IO (Maybe Cliente)
buscarCpfRegistrado cpfEntrada = do
    (clienteMaybe, _) <- buscarClienteEAutomovel cpfEntrada ""
    return clienteMaybe

-- Função para buscar se a placa já está registrada
buscarPlacaRegistrada :: String -> IO (Maybe Automovel)
buscarPlacaRegistrada placaEntrada = do
    (_, automovelMaybe) <- buscarClienteEAutomovel "" placaEntrada
    return automovelMaybe

-- Função auxiliar para cadastro de cliente
cadastrarClienteMain :: IO ()
cadastrarClienteMain = do
    cpf <- solicitarCpf
    clienteExistente <- buscarCpfRegistrado cpf

    case clienteExistente of
        Just cliente -> do
            putStrLn $ "Cliente encontrado! Nome: " ++ nome cliente
            putStrLn "Cadastramento encerrado."
        Nothing -> do
            putStrLn "CPF não registrado. Prosseguindo com o cadastro..."
            putStrLn "Digite o nome do cliente:"
            nome <- getLine
            telefone <- solicitarTelefone
            putStrLn "Digite o sexo do cliente:"
            sexo <- getLine
            putStrLn "Digite a data de nascimento do cliente (formato: YYYY-MM-DD):"
            dataNascimentoStr <- getLine
            let dataNascimento = read dataNascimentoStr :: Day
            idade <- calcularIdade dataNascimento
            putStrLn "Digite o modelo do automóvel:"
            modelo <- getLine
            putStrLn "Digite o ano do automóvel:"
            anoStr <- getLine
            let ano = read anoStr :: Int
            putStrLn "Digite o tipo do veículo (Carro ou Moto):"
            tipoVeiculo <- getLine
            placa <- solicitarPlacaMercosul
            placaExistente <- buscarPlacaRegistrada placa

            case placaExistente of
                Just _ -> putStrLn "Placa já registrada. Cadastramento encerrado."
                Nothing -> do
                    let nivelCliente = 2
                    let (cliente, automovel) = cadastrarCliente nome cpf telefone sexo idade modelo ano placa tipoVeiculo nivelCliente
                    salvarCliente cliente automovel
                    putStrLn "\nCliente cadastrado:"
                    print cliente
                    putStrLn "\nAutomóvel cadastrado:"
                    print automovel

-- Função para cadastrar um seguro
cadastrarSeguroMain :: IO ()
cadastrarSeguroMain = do
    cpf <- solicitarCpf
    clienteMaybe <- buscarCpfRegistrado cpf

    case clienteMaybe of
        Nothing -> putStrLn "Cliente não encontrado. Realize o cadastro do cliente primeiro."
        Just cliente -> do
            putStrLn "Digite o nome do cliente:"
            nomeInput <- getLine
            if nome cliente /= nomeInput
                then putStrLn "Cadastramento impossibilitado! Nome não corresponde ao CPF."
                else do
                    putStrLn "Digite o tipo de contrato (Básico, Tradicional, Premium):"
                    tipoContrato <- getLine
                    putStrLn "Digite o valor do seguro:"
                    valorSeguroStr <- getLine
                    let valorSeguro = read valorSeguroStr :: Float
                    putStrLn "Digite a placa do veículo a ser assegurado:"
                    placa <- solicitarPlacaMercosul
                    automovelMaybe <- buscarPlacaRegistrada placa
                    
                    case automovelMaybe of
                        Nothing -> putStrLn "Automóvel não encontrado. Certifique-se de que o veículo esteja cadastrado."
                        Just automovel -> do
                            seguro <- criarSeguro cliente automovel tipoContrato valorSeguro
                            salvarSeguro seguro
                            putStrLn "\nSeguro criado com sucesso:"
                            print seguro


main :: IO ()
main = do
    menu