-- Modulo que contem funções auxiliares para o funcionamento do programa
module Utils () where

import Data.Time (Day, getCurrentTime, utctDay, parseTimeOrError, defaultTimeLocale, toGregorian)
import Data.Char (isDigit, isAlpha)
import Data.Time (Day, getCurrentTime, utctDay, toGregorian)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Tipos



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
buscarPlacaRegistrada :: String -> IO (Maybe Veiculo)
buscarPlacaRegistrada placaEntrada = do
    (_, automovelMaybe) <- buscarClienteEAutomovel "" placaEntrada
    return automovelMaybe

-- Função para validar se a placa está no padrão Mercosul
validarPlacaMercosul :: String -> Bool
validarPlacaMercosul placa =
    length placa == 7 &&
    all isAlpha (take 3 placa) &&    -- Primeiras 3 letras devem ser alfabéticas
    isDigit (placa !! 3) &&          -- O 4º caractere deve ser um dígito
    isAlpha (placa !! 4) &&          -- O 5º caractere deve ser uma letra
    all isDigit (drop 5 placa)       -- Últimos dois caracteres devem ser dígitos

-- Função para solicitar o CPF com validação
solicitarCpf :: IO String
solicitarCpf = do
    putStrLn "Digite o CPF do cliente (11 dígitos):"
    cpf <- getLine
    if length cpf == 11 && all isDigit cpf
        then return cpf
        else do
            putStrLn "CPF inválido. Tente novamente."
            solicitarCpf

-- Função para solicitar o telefone com validação
solicitarTelefone :: IO String
solicitarTelefone = do
    putStrLn "Digite o telefone do cliente (11 dígitos):"
    telefone <- getLine
    if length telefone == 11 && all isDigit telefone
        then return telefone
        else do
            putStrLn "Telefone inválido. Tente novamente."
            solicitarTelefone

-- Função para solicitar a placa no padrão Mercosul
solicitarPlacaMercosul :: IO String
solicitarPlacaMercosul = do
    putStrLn "Digite a placa do automóvel no padrão Mercosul (ABC1D23):"
    placa <- getLine
    if validarPlacaMercosul placa
        then return placa
        else do
            putStrLn "Placa inválida. Tente novamente."
            solicitarPlacaMercosul

-- Função para ler uma linha e converter para Cliente e Automovel
parseLine :: String -> (Cliente, Veiculo)
parseLine linha = 
    let (clienteStr, automovelStr) = break (== '|') linha
        cliente = parseCliente (init clienteStr)
        automovel = parseAutomovel (tail automovelStr)
    in (cliente, automovel)

-- Função auxiliar que faz parsing manual do cliente
parseCliente :: String -> Cliente
parseCliente str =
    let campos = split ',' (removeWrapper "Cliente" str)
        nome = removeQuotes $ extractValue "nome" (campos !! 0)
        cpf = removeQuotes $ extractValue "cpf" (campos !! 1)
        telefone = removeQuotes $ extractValue "telefone" (campos !! 2)
        sexo = removeQuotes $ extractValue "sexo" (campos !! 3)
        idade = read $ extractValue "idade" (campos !! 4) :: Int
    in Cliente cpf nome telefone sexo idade "" 2 True 0 0 (Veiculo "" 0 "" "")

-- Função auxiliar que faz parsing manual do automóvel
parseAutomovel :: String -> Veiculo
parseAutomovel str =
    let campos = split ',' (removeWrapper "Automovel" str)
        modelo = removeQuotes $ extractValue "modelo" (campos !! 0)
        ano = read $ extractValue "ano" (campos !! 1) :: Int
        placa = removeQuotes $ extractValue "placa" (campos !! 2)
        tipoAutomovel = removeQuotes $ extractValue "tipoAutomovel" (campos !! 3)
    in Veiculo modelo ano placa tipoAutomovel

parseSinistro :: String -> Sinistro
parseSinistro str = 
    let fields = split ',' str
    in Sinistro 
        (fields !! 0)             -- idSinistro
        (fields !! 1)             -- cpfClienteSinistro
        (fields !! 2)             -- idSeguroSinistro
        (fields !! 3)             -- nivelAcidente
        (read (fields !! 4))      -- custoSinistro (convertendo de String para Float)
        (parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (fields !! 5) :: Day) -- dataSinistro
        (read (fields !! 6))      -- ativoSinistro (convertendo de String para Bool)


-- Função auxiliar para remover a parte "Cliente {...}" ou "Automovel {...}" e deixar apenas os valores internos
removeWrapper :: String -> String -> String
removeWrapper wrapper str = take (length str - length wrapper - 3) (drop (length wrapper + 2) str)

-- Função auxiliar para remover aspas de uma string
removeQuotes :: String -> String
removeQuotes = filter (/= '"')

-- Função auxiliar para extrair o valor de um campo no formato "campo = valor"
extractValue :: String -> String -> String
extractValue campo str = drop (length campo + 4) str  -- Remove "campo = "

-- Função para dividir uma string por um delimitador
split :: Char -> String -> [String]
split _ "" = []
split delim s = let (before, remainder) = span (/= delim) s
                 in before : case remainder of
                                [] -> []
                                x -> split delim (tail x)

-- TO DO: Implementar a função de limpar a tela para o seu sistema operacional
limparTela :: IO ()
limparTela = putStrLn "\ESC[2J"
