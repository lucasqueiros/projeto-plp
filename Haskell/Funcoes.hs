module Funcoes where

import Control.Concurrent (threadDelay)
import Tipos
import System.IO
import Data.List (find, isInfixOf)
import Data.Time (Day, getCurrentTime, utctDay, parseTimeOrError, defaultTimeLocale, toGregorian)
import Data.Char (isDigit, isAlpha)
import Data.Char (toLower)

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

-- Função auxiliar para calcular a idade com base na data de nascimento
calcularIdade' :: Day -> IO Int
calcularIdade' dataNascimento = do
    currentDate <- utctDay <$> getCurrentTime
    let (anoAtual, mesAtual, diaAtual) = toGregorian currentDate
        (anoNasc, mesNasc, diaNasc) = toGregorian dataNascimento
        idade = fromInteger (anoAtual - anoNasc) - if (mesAtual, diaAtual) < (mesNasc, diaNasc) then 1 else 0
    return idade

-- Função para editar um cliente existente
editarClienteMain :: IO ()
editarClienteMain = do
    cpf <- solicitarCpf
    clienteExistente <- buscarClienteEAutomovel cpf ""

    case clienteExistente of
        (Nothing, _) -> putStrLn "Cliente não encontrado."
        (Just cliente, Just automovel) -> do
            putStrLn "Cliente encontrado! Atributos disponíveis para edição:"
            putStrLn "1 - Nome"
            putStrLn "2 - Telefone"
            putStrLn "3 - Sexo"
            putStrLn "4 - Data de Nascimento"
            putStrLn "5 - Modelo do Automóvel"
            putStrLn "6 - Ano do Automóvel"
            putStrLn "7 - Placa do Automóvel"
            putStrLn "8 - Tipo do Veículo"
            putStrLn "Escolha o número do atributo que deseja editar:"
            opcao <- getLine

            -- Editar atributos do cliente
            novoCliente <- case opcao of
                "1" -> do
                    putStrLn "Digite o novo nome:"
                    novoNome <- getLine
                    return cliente { nome = novoNome }
                "2" -> do
                    novoTelefone <- solicitarTelefone
                    return cliente { telefone = novoTelefone }
                "3" -> do
                    putStrLn "Digite o novo sexo:"
                    novoSexo <- getLine
                    return cliente { sexo = novoSexo }
                "4" -> do
                    putStrLn "Digite a nova data de nascimento (formato: YYYY-MM-DD):"
                    novaDataStr <- getLine
                    let novaData = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" novaDataStr :: Day
                    novaIdade <- calcularIdade' novaData
                    return cliente { idade = novaIdade }
                "5" -> do
                    putStrLn "Digite o novo modelo do automóvel:"
                    novoModelo <- getLine
                    let novoAutomovel = automovel { modelo = novoModelo }
                    return cliente { veiculo = novoAutomovel }
                "6" -> do
                    putStrLn "Digite o novo ano do automóvel:"
                    novoAnoStr <- getLine
                    let novoAno = read novoAnoStr :: Int
                    let novoAutomovel = automovel { ano = novoAno }
                    return cliente { veiculo = novoAutomovel }
                "7" -> do
                    putStrLn "Digite a nova placa do automóvel:"
                    novaPlaca <- solicitarPlacaMercosul
                    let novoAutomovel = automovel { placa = novaPlaca }
                    return cliente { veiculo = novoAutomovel }
                "8" -> do
                    putStrLn "Digite o novo tipo de veículo (Carro ou Moto):"
                    novoTipoAutomovel <- getLine
                    let novoAutomovel = automovel { tipoAutomovel = novoTipoAutomovel }
                    return cliente { veiculo = novoAutomovel }
                _ -> return cliente

            -- Atualiza o arquivo de clientes
            atualizarCliente cpf novoCliente

            putStrLn "Cliente atualizado com sucesso."

-- Função para atualizar um cliente no arquivo clientes.txt
atualizarCliente :: String -> Cliente -> IO ()
atualizarCliente cpfAtualizado novoCliente = do
    -- Força a leitura completa do conteúdo antes de prosseguir
    conteudo <- withFile "dados/clientes.txt" ReadMode (\handle -> do
        conteudo <- hGetContents handle
        length conteudo `seq` return conteudo)  -- Força a avaliação completa do conteúdo

    let linhas = lines conteudo
        novaLinha = clienteToString novoCliente ++ " | " ++ automovelToString (veiculo novoCliente)
        linhasAtualizadas = map (\linha -> if cpfAtualizado `isInfixOf` linha then novaLinha else linha) linhas
    
    -- Escreve o conteúdo atualizado de volta ao arquivo
    withFile "dados/clientes.txt" WriteMode $ \handleEscrita -> do
        hPutStr handleEscrita (unlines linhasAtualizadas)
    putStrLn "Cliente atualizado com sucesso."


-- Função para listar todos os clientes e seus automóveis em uma tabela
listarClientes :: IO ()
listarClientes = do
    conteudo <- readFile "dados/clientes.txt"
    let linhas = lines conteudo
        clientesAutomoveis = map parseLine linhas
    putStrLn "==================================================================================="
    putStrLn "| Nome do Cliente         | Modelo do Carro    | Placa do Carro | Nível do Cliente |"
    putStrLn "==================================================================================="
    mapM_ (putStrLn . formatarCliente) clientesAutomoveis
    putStrLn "==================================================================================="

-- Função auxiliar para formatar os dados de um cliente e seu automóvel em uma linha de tabela
formatarCliente :: (Cliente, Automovel) -> String
formatarCliente (cliente, automovel) =
    let nomeCliente = take 22 $ nome cliente ++ replicate 22 ' '
        modeloCarro = take 18 $ modelo automovel ++ replicate 18 ' '
        placaCarro = take 15 $ placa automovel ++ replicate 15 ' '
        nivelClienteStr = take 10 $ show (nivelCliente cliente) ++ replicate 10 ' '
    in "| " ++ nomeCliente ++ " | " ++ modeloCarro ++ " | " ++ placaCarro ++ " | " ++ nivelClienteStr ++ " |"

-- Função que recebe todos os dados do cliente e instancia um tipo Cliente
cadastrarCliente :: String -> String -> String -> String -> Int -> String -> Int -> String -> String -> Int -> (Cliente, Automovel)
cadastrarCliente nome' cpf' telefone' sexo' idade' modelo' ano' placa' tipoAutomovel' nivelCliente' =
    let cliente = Cliente nome' cpf' telefone' sexo' idade' nivelCliente' (Automovel modelo' ano' placa' tipoAutomovel')
        automovel = veiculo cliente
    in (cliente, automovel)

-- Função para salvar o cliente em um arquivo de texto
salvarCliente :: Cliente -> Automovel -> IO ()
salvarCliente cliente automovel = do
    let clienteStr = clienteToString cliente
    let automovelStr = automovelToString automovel
    appendFile "dados/clientes.txt" (clienteStr ++ " | " ++ automovelStr ++ "\n")
    putStrLn "Cliente e automóvel registrados no banco de dados."

-- Função para converter Cliente para String
clienteToString :: Cliente -> String
clienteToString (Cliente nome cpf telefone sexo idade _ _) =
    "Cliente {nome = \"" ++ nome ++ "\", cpf = \"" ++ cpf ++ "\", telefone = \"" ++ telefone ++ "\", sexo = \"" ++ sexo ++ "\", idade = " ++ show idade ++ "}"

-- Função para converter Automovel para String
automovelToString :: Automovel -> String
automovelToString (Automovel modelo ano placa tipoAutomovel) =
    "Automovel {modelo = \"" ++ modelo ++ "\", ano = " ++ show ano ++ ", placa = \"" ++ placa ++ "\", tipoAutomovel = \"" ++ tipoAutomovel ++ "\"}"

-- Função para buscar cliente e automóvel por CPF e placa
buscarClienteEAutomovel :: String -> String -> IO (Maybe Cliente, Maybe Automovel)
buscarClienteEAutomovel cpfEntrada placaEntrada = do
    conteudo <- readFile "dados/clientes.txt"
    let cpfEntradaTrim = filter (/= ' ') cpfEntrada
        placaEntradaTrim = filter (/= ' ') placaEntrada
    let linhas = lines conteudo
        clienteAutomovel = map parseLine linhas
        clienteEncontrado = find (\(cliente, automovel) -> filter (/= ' ') (cpf cliente) == cpfEntradaTrim || filter (/= ' ') (placa automovel) == placaEntradaTrim) clienteAutomovel
    case clienteEncontrado of
        Just (cliente, automovel) -> return (Just cliente, Just automovel)
        Nothing -> return (Nothing, Nothing)

-- Função para ler uma linha e converter para Cliente e Automovel
parseLine :: String -> (Cliente, Automovel)
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
    in Cliente nome cpf telefone sexo idade 2 (Automovel "" 0 "" "")

-- Função auxiliar que faz parsing manual do automóvel
parseAutomovel :: String -> Automovel
parseAutomovel str =
    let campos = split ',' (removeWrapper "Automovel" str)
        modelo = removeQuotes $ extractValue "modelo" (campos !! 0)
        ano = read $ extractValue "ano" (campos !! 1) :: Int
        placa = removeQuotes $ extractValue "placa" (campos !! 2)
        tipoAutomovel = removeQuotes $ extractValue "tipoAutomovel" (campos !! 3)
    in Automovel modelo ano placa tipoAutomovel

-- Função auxiliar para remover a parte "Cliente {...}" ou "Automovel {...}" e deixar apenas os valores internos
removeWrapper :: String -> String -> String
removeWrapper wrapper str = take (length str - length wrapper - 3) (drop (length wrapper + 2) str)

-- Função auxiliar para remover aspas de uma string
removeQuotes :: String -> String
removeQuotes = filter (/= '"')

-- Função auxiliar para extrair o valor de um campo no formato "campo = valor"
extractValue :: String -> String -> String
extractValue campo str = drop (length campo + 4) str  -- Remove "campo = "

-- Função para criar um seguro e salvar no arquivo
criarSeguro :: Cliente -> Automovel -> String -> Float -> IO Seguro
criarSeguro cliente automovel tipoContrato valorSeguro = do
    dataAtual <- utctDay <$> getCurrentTime
    let seguro = Seguro cliente automovel tipoContrato valorSeguro dataAtual
    salvarSeguro seguro
    return seguro

-- Função para salvar o seguro em um arquivo de texto
salvarSeguro :: Seguro -> IO ()
salvarSeguro seguro = do
    let seguroStr = seguroToString seguro
    appendFile "dados/seguros.txt" (seguroStr ++ "\n")
    putStrLn "Seguro registrado no banco de dados."

-- Função para converter Seguro para String
seguroToString :: Seguro -> String
seguroToString (Seguro cliente automovel contratoTipo valorSeguro dataEmissao) =
    clienteToString cliente ++ " | " ++ automovelToString automovel ++ ", contratoTipo = \"" ++ contratoTipo ++ "\", valorSeguro = " ++ show valorSeguro ++ ", dataEmissao = " ++ show dataEmissao

-- Função para dividir uma string por um delimitador
split :: Char -> String -> [String]
split _ "" = []
split delim s = let (before, remainder) = span (/= delim) s
                 in before : case remainder of
                                [] -> []
                                x -> split delim (tail x)

-- Função responsável por calcular o nível do cliente, mas sem atualiza-lo no cliente   
calcularNivelCliente :: Cliente -> Int
calcularNivelCliente cliente
        | not (statusFinanceiro cliente) = 1
        | otherwise = max 1 (min 5 (nivelBase - penalidade))
        where
            nivelBase = 2 + (tempoFidelidade cliente `div` 6)
            penalidade = numSinistros cliente `div` 2

-- Função responsável por atualizar o nível do cliente (essa deve ser chamada)
atualizarNivelCliente :: Cliente -> Cliente
atualizarNivelCliente cliente = cliente { nivel = calcularNivelCliente cliente }

--vai receber o custo do sinistro e um inteiro do nivel do cliente e retorna o valor do desconto
calcularDesconto :: Float -> Int -> Float
calcularDesconto custoSinistro nivelCliente
    | nivelCliente == 1 = custoSinistro
    | nivelCliente == 2 = custoSinistro * 0.1
    | nivelCliente == 3 = custoSinistro * 0.15
    | nivelCliente == 4 = custoSinistro * 0.2
    | nivelCliente == 5 = custoSinistro * 0.25
    | otherwise = error "Nivel de Cliente invalido!"

--vai receber o custo do sinistro e um inteiro do nivel do cliente e retorna o valor do desconto
calcularValorDescontado :: Float -> Int -> Float
calcularValorDescontado custoSinistro nivelCliente
    | nivelCliente == 1 = custoSinistro
    | nivelCliente == 2 = custoSinistro - (custoSinistro * 0.1)
    | nivelCliente == 3 = custoSinistro - (custoSinistro * 0.15)
    | nivelCliente == 4 = custoSinistro - (custoSinistro * 0.2)
    | nivelCliente == 5 = custoSinistro - (custoSinistro * 0.25)
    | otherwise = error "Nivel de Cliente invalido!"

--limpar a tela
limparTela :: String
limparTela = ""


--cadastrarSinistro
cadastrarSinistro :: String -> String -> String -> String -> Float -> String -> IO ()
cadastrarSinistro idS cpfS idSegS nivelA custoS dataS = do
    let sinistro = Sinistro idS cpfS idSegS nivelA custoS dataS True
    appendFile "dados/sinistros.txt" (show sinistro ++ "\n")
    putStrLn "Sinistro cadastrado com sucesso."

--encerrarSinistro
encerrarSinistro :: String -> IO ()
encerrarSinistro idS = do
    conteudo <- readFile "dados/sinistros.txt"
    let sinistros = map read (lines conteudo) :: [Sinistro]
        sinistrosAtualizados = map (\s -> if idSinistro s == idS then s {ativo = False} else s) sinistros
    writeFile "dados/sinistros.txt" (unlines (map show sinistrosAtualizados))
    putStrLn "Sinistro encerrado com sucesso."

--listarSinistro
listarSinistrosAtivos :: IO ()
listarSinistrosAtivos = do
    conteudo <- readFile "dados/sinistros.txt"
    let sinistros = map read (lines conteudo) :: [Sinistro]
        sinistrosAtivos = filter ativo sinistros
    mapM_ (putStrLn . show) sinistrosAtivos

-- Recebe na seguinte ordem, tipo do seguro (basico, intermediario ou premium), idade, estado civil e sexo.
simularSeguro :: String -> Int -> String -> String -> String
simularSeguro tipoSeguro idade estadoCivil sexo
    | tipoSeguroNormalizado == "basico" = "O seu seguro (" ++ tipoSeguro ++ ") fica por um valor de " ++ show (200 * simularRisco idade estadoCivil sexo) ++ " mensalmente."
    | tipoSeguroNormalizado == "intermediario" = "O seu seguro (" ++ tipoSeguro ++ ") fica por um valor de " ++ show (300 * simularRisco idade estadoCivil sexo) ++ " mensalmente."
    | tipoSeguroNormalizado == "premium" = "O seu seguro (" ++ tipoSeguro ++ ") fica por um valor de " ++ show (500 * simularRisco idade estadoCivil sexo) ++ " mensalmente."
    | otherwise = "Tipo de seguro inválido."
  where
    tipoSeguroNormalizado = map toLower tipoSeguro


--simula o risco do seguro baseado em atributos do cliente
--recebe idade, estado civil e sexo.
simularRisco :: Int -> String -> String -> Float
simularRisco idade estadoCivil sexo =
    idadeRisco idade * estadoCivilRisco estadoCivil * sexoRisco sexo

--Funcao auxiliar para calcular o ajuste de risco baseado na idade
idadeRisco :: Int -> Float
idadeRisco idade
    | idade <= 23 = 1.05  -- Jovem (23 anos ou menos) - 5% mais caro
    | idade >= 65 = 1.05  -- Idoso (acima de 65 anos) - 5% mais caro
    | otherwise = 1.00  -- Adulto - nenhum ajuste

--Funcao auxiliar para calcular o ajuste de risco baseado no estado civil
estadoCivilRisco :: String -> Float
estadoCivilRisco estadoCivil
    | estadoCivilNormalizado == "solteiro" = 1.025  -- Solteiro - 2.5% mais caro
    | estadoCivilNormalizado == "casado"   = 1.00   -- Casado - nenhum ajuste
    | otherwise = 1.00   -- Nenhum ajuste para outros casos
  where
    estadoCivilNormalizado = map toLower estadoCivil

--Funcao auxiliar para calcular o ajuste de risco baseado no sexo
sexoRisco :: String -> Float
sexoRisco sexo
    | sexoNormalizado == "homem"  || sexoNormalizado == "h" = 1.05  -- Homem - 5% mais caro
    | sexoNormalizado == "mulher" || sexoNormalizado == "m" = 1.00  -- Mulher - nenhum ajuste
    | otherwise = 1.00  -- Nenhum ajuste para outros casos
  where
    sexoNormalizado = map toLower sexo
