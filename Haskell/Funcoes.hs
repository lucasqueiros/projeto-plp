module Funcoes (
    cadastrarClienteMain,
    editarClienteMain,
    cadastrarSeguroMain,
    listarClientes,
    mostrarStatusPagamento
) where

import Control.Concurrent (threadDelay)
import Tipos
import System.IO
import Data.List (find, isInfixOf)
import Data.Time (Day, getCurrentTime, utctDay, parseTimeOrError, defaultTimeLocale, toGregorian)
import Data.Char (isDigit, isAlpha)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Char (isDigit, isAlpha)
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
buscarPlacaRegistrada :: String -> IO (Maybe Veiculo)
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
            putStrLn $ "Cliente encontrado! Nome: " ++ nomeCliente cliente
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
                    putStrLn "\nCliente cadastrado com sucesso!"
                    

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
            if nomeCliente cliente /= nomeInput
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
                    return cliente { nomeCliente = novoNome }
                "2" -> do
                    novoTelefone <- solicitarTelefone
                    return cliente { telefoneCliente = novoTelefone }
                "3" -> do
                    putStrLn "Digite o novo sexo:"
                    novoSexo <- getLine
                    return cliente { sexoCliente = novoSexo }
                "4" -> do
                    putStrLn "Digite a nova data de nascimento (formato: YYYY-MM-DD):"
                    novaDataStr <- getLine
                    let novaData = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" novaDataStr :: Day
                    novaIdade <- calcularIdade' novaData
                    return cliente { idadeCliente = novaIdade }
                "5" -> do
                    putStrLn "Digite o novo modelo do automóvel:"
                    novoModelo <- getLine
                    let novoAutomovel = Veiculo { modeloVeiculo = novoModelo, anoVeiculo = anoVeiculo automovel, tipoVeiculo = tipoVeiculo automovel, placaVeiculo = placaVeiculo automovel }
                    return cliente { veiculoCliente = novoAutomovel }
                "6" -> do
                    putStrLn "Digite o novo ano do automóvel:"
                    novoAnoStr <- getLine
                    let novoAno = read novoAnoStr :: Int
                    let novoAutomovel = automovel { anoVeiculo = novoAno }
                    return cliente { veiculoCliente = novoAutomovel }
                "7" -> do
                    putStrLn "Digite a nova placa do automóvel:"
                    novaPlaca <- solicitarPlacaMercosul
                    let novoAutomovel = automovel { placaVeiculo = novaPlaca }
                    return cliente { veiculoCliente = novoAutomovel }
                "8" -> do
                    putStrLn "Digite o novo tipo de veículo (Carro ou Moto):"
                    novoTipoAutomovel <- getLine
                    let novoAutomovel = automovel { tipoVeiculo = novoTipoAutomovel }
                    return cliente { veiculoCliente = novoAutomovel }
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
        novaLinha = clienteToString novoCliente ++ " | " ++ automovelToString (veiculoCliente novoCliente)
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
formatarCliente :: (Cliente, Veiculo) -> String
formatarCliente (cliente, automovel) =
    let nomeClienteFormatado = take 22 $ padRight 22 (nomeCliente cliente)
        modeloCarroFormatado = take 18 $ padRight 18 (modeloVeiculo automovel)
        placaCarroFormatado = take 15 $ padRight 15 (placaVeiculo automovel)
        nivelClienteStrFormatado = take 10 $ padRight 10 (show (nivelCliente cliente))
    in "| " ++ nomeClienteFormatado ++ " | " ++ modeloCarroFormatado ++ " | " ++ placaCarroFormatado ++ " | " ++ nivelClienteStrFormatado ++ " |"

-- Função auxiliar para preencher uma string com espaços à direita até um determinado comprimento
padRight :: Int -> String -> String
padRight n str = str ++ replicate (n - length str) ' '

-- Função que recebe todos os dados do cliente e instancia um tipo Cliente
cadastrarCliente :: String -> String -> String -> String -> Int -> String -> Int -> String -> String -> Int -> (Cliente, Veiculo)
cadastrarCliente nome cpf telefone sexo idade modelo ano placa tipoVeiculo nivelCliente =
    let veiculo = Veiculo modelo ano tipoVeiculo placa
        cliente = Cliente cpf nome telefone sexo idade "" nivelCliente True 0 0 veiculo
    in (cliente, veiculo)


-- Função para salvar o cliente em um arquivo de texto
salvarCliente :: Cliente -> Veiculo -> IO ()
salvarCliente cliente automovel = do
    let clienteStr = clienteToString cliente
    let automovelStr = automovelToString automovel
    appendFile "dados/clientes.txt" (clienteStr ++ " | " ++ automovelStr ++ "\n")
    putStrLn "Cliente e automóvel registrados no banco de dados."

-- Função para converter Cliente para String
clienteToString :: Cliente -> String
clienteToString (Cliente cpf nome telefone sexo idade _ nivelCliente _ _ _ veiculo) =
    "Cliente {nome = \"" ++ nome ++ "\", cpf = \"" ++ cpf ++ "\", telefone = \"" ++ telefone ++ "\", sexo = \"" ++ sexo ++ "\", idade = " ++ show idade ++ "}"

-- Função para converter Automovel para String
automovelToString :: Veiculo -> String
automovelToString (Veiculo modelo ano tipoVeiculo placa) =
    "Veiculo {modelo = \"" ++ modelo ++ "\", ano = " ++ show ano ++ ", tipoVeiculo = \"" ++ tipoVeiculo ++ "\", placa = \"" ++ placa ++ "\"}"

-- Função para buscar cliente e automóvel por CPF e placa
buscarClienteEAutomovel :: String -> String -> IO (Maybe Cliente, Maybe Veiculo)
buscarClienteEAutomovel cpfEntrada placaEntrada = do
    conteudo <- readFile "dados/clientes.txt"
    let cpfEntradaTrim = filter (/= ' ') cpfEntrada
        placaEntradaTrim = filter (/= ' ') placaEntrada
    let linhas = lines conteudo
        clienteAutomovel = map parseLine linhas
        clienteEncontrado = find (\(cliente, automovel) -> filter (/= ' ') (cpfCliente cliente) == cpfEntradaTrim || filter (/= ' ') (placaVeiculo automovel) == placaEntradaTrim) clienteAutomovel
    case clienteEncontrado of
        Just (cliente, automovel) -> return (Just cliente, Just automovel)
        Nothing -> return (Nothing, Nothing)

-- Função para ler uma linha e converter para Cliente e Automovel
parseLine :: String -> (Cliente, Veiculo)
parseLine linha = 
    let (clienteStr, automovelStr) = break (== '|') linha
        cliente = parseCliente (init clienteStr)
        automovel = parseAutomovel (tail automovelStr)
    in (cliente, automovel)

-- Função auxiliar para acessar um elemento de uma lista de forma segura
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
    | i < 0 || i >= length xs = Nothing
    | otherwise = Just (xs !! i)

-- Parsing manual do cliente com extração adequada
parseCliente :: String -> Cliente
parseCliente str =
    let campos = split ',' (removeWrapper "Cliente" str)
        nome = removeQuotes (extractValue "nome" (campos !! 0))
        cpf = removeQuotes (extractValue "cpf" (campos !! 1))
        telefone = removeQuotes (extractValue "telefone" (campos !! 2))
        sexo = removeQuotes (extractValue "sexo" (campos !! 3))
        idade = read (extractValue "idade" (campos !! 4)) :: Int
    in Cliente cpf nome telefone sexo idade "" 2 True 0 0 (Veiculo "" 0 "" "")

-- Parsing manual do automóvel com extração adequada
parseAutomovel :: String -> Veiculo
parseAutomovel str =
    let campos = split ',' (removeWrapper "Veiculo" str)
        modelo = removeQuotes $ extractValue "modelo" (campos !! 0)
        ano = read $ extractValue "ano" (campos !! 1) :: Int
        tipoVeiculo = removeQuotes $ extractValue "tipoVeiculo" (campos !! 2)
        -- Extrair a placa corretamente, removendo qualquer prefixo extra
        placa = removeQuotes $ dropWhile (/= '=') (extractValue "placa" (campos !! 3)) 
    in Veiculo modelo ano tipoVeiculo (drop 2 placa)  -- Remover " = " do início da placa

parseSinistro :: String -> Sinistro
parseSinistro str = 
    let fields = split ',' str
    in Sinistro 
        (fields !! 0)             -- idSinistro
        (fields !! 1)             -- cpfClienteSinistro
        (fields !! 2)             -- idSeguroSinistro
        (fields !! 3)             -- nivelAcidente
        (read (fields !! 4))      -- custoSinistro (convertendo de String para Float)
        (read (fields !! 5))      -- dataSinistro (convertendo de String para Day)
        (read (fields !! 6))      -- ativoSinistro (convertendo de String para Bool)


-- Corrigindo a função removeWrapper para remover o wrapper corretamente
removeWrapper :: String -> String -> String
removeWrapper wrapper str = 
    let start = length wrapper + 2
        end = length str - 1
    in if start <= end then take (end - start) (drop start str) else ""

-- Função auxiliar para remover aspas de uma string
removeQuotes :: String -> String
removeQuotes = filter (/= '"')

-- Função auxiliar para extrair o valor de um campo no formato "campo = valor"
extractValue :: String -> String -> String
extractValue campo str =
    let searchStr = campo ++ " = "
        rest = drop (length searchStr) str
    in takeWhile (/= ',') rest

-- Função para criar um seguro e salvar no arquivo
criarSeguro :: Cliente -> Veiculo -> String -> Float -> IO Seguro
criarSeguro cliente veiculo tipoContrato valorSeguro = do
    dataAtual <- utctDay <$> getCurrentTime
    let seguro = Seguro cliente veiculo tipoContrato valorSeguro dataAtual
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
seguroToString (Seguro cliente veiculo contratoTipo valorSeguro dataEmissao) =
    clienteToString cliente ++ " | " ++ automovelToString veiculo ++ ", contratoTipo = \"" ++ contratoTipo ++ "\", valorSeguro = " ++ show valorSeguro ++ ", dataEmissao = " ++ show dataEmissao

-- Função para dividir uma string por um delimitador
split :: Char -> String -> [String]
split _ "" = []
split delim s = 
    let (before, remainder) = span (/= delim) s
    in before : case remainder of
        [] -> []
        x -> split delim (tail x)

-- Função responsável por calcular o nível do cliente, mas sem atualiza-lo no cliente   
calcularNivelCliente :: Cliente -> Int
calcularNivelCliente cliente
        | not (statusFinanceiroCliente cliente) = 1
        | otherwise = max 1 (min 5 (nivelBase - penalidade))
        where
            nivelBase = 2 + (tempoFidelidadeCliente cliente `div` 6)
            penalidade = numSinistrosCliente cliente `div` 2

-- Função responsável por atualizar o nível do cliente (essa deve ser chamada)
atualizarNivelCliente :: Cliente -> Cliente
atualizarNivelCliente cliente = cliente { nivelCliente = calcularNivelCliente cliente }

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
    let sinistro = Sinistro idS cpfS idSegS nivelA custoS (read dataS :: Day) True
    appendFile "dados/sinistros.txt" (show sinistro ++ "\n")
    putStrLn "Sinistro cadastrado com sucesso."

-- Função encerrarSinistro após a modificação
encerrarSinistro :: String -> IO ()
encerrarSinistro idS = do
    conteudo <- readFile "dados/sinistros.txt"
    let sinistros = map parseSinistro (lines conteudo)
        sinistrosAtualizados = map (\s -> if idSinistro s == idS then s {ativoSinistro = False} else s) sinistros
    writeFile "dados/sinistros.txt" (unlines (map show sinistrosAtualizados))
    putStrLn "Sinistro encerrado com sucesso."

--listarSinistro
listarSinistrosAtivos :: IO ()
listarSinistrosAtivos = do
    conteudo <- readFile "dados/sinistros.txt"
    let sinistros = map parseSinistro (lines conteudo) :: [Sinistro]
        sinistrosAtivos = filter ativoSinistro sinistros
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


--arquivos funcoes2

-- Função que busca um cliente pelo CPF
buscarClientePorCpf :: String -> IO (Maybe Cliente)
buscarClientePorCpf cpfEntrada = do
    conteudo <- readFile "dados/clientes.txt"
    let linhas = lines conteudo
        clientes = map parseCliente linhas
        clienteEncontrado = find (\cliente -> cpfCliente cliente == cpfEntrada) clientes
    return clienteEncontrado
--

-- Função para mostrar o status de pagamento
mostrarStatusPagamento :: IO ()
mostrarStatusPagamento = do
    cpf <- solicitarCpf
    cliente <- buscarClientePorCpf cpf
    case cliente of
        Just c  -> putStrLn $ pagamentoEmDia (statusFinanceiroCliente c)
        Nothing -> putStrLn "Cliente não encontrado."

-- Função que Verifica o Status Financeiro do Seguro
pagamentoEmDia :: Bool -> String 
pagamentoEmDia status = 
    if status
        then "Em dia!"
    else "Devendo!"

-- Função para formatar um Sinistro como uma String
formatarSinistro :: Sinistro -> String
formatarSinistro sinistro =
    intercalate " | " [idSinistro sinistro, nivelAcidente sinistro, show (custoSinistro sinistro), show (dataSinistro sinistro)]


-- Função que gera um relatório com com informações acerca de um cliente registrado.
gerarRelatorio :: Cliente -> [Sinistro] -> String
gerarRelatorio cliente sinistros =
    let
        seguro = head (listaSegurosCliente cliente)
        -- Informações pessoais do cliente e de seu automóvel.
        infoCliente = "Nome: " ++ nomeCliente cliente ++ "\n" ++
                      "CPF: " ++ cpfCliente cliente ++ "\n" ++
                      "Telefone: " ++ telefoneCliente cliente ++ "\n" ++
                      "Idade: " ++ show (idadeCliente cliente) ++ "\n" ++
                      "Sexo: " ++ sexoCliente cliente ++ "\n" ++
                      "Estado Civil: " ++ estadoCivilCliente cliente ++ "\n"

        -- Detalhes do seguro do cliente. 
        infoNivel = "Nível de Cliente: " ++ show (nivelCliente cliente) ++ "\n" ++
                    "Tipo de Contrato: " ++ tipoContratoSeguro seguro ++ "\n" ++
                    "Status Financeiro: " ++ pagamentoEmDia (statusFinanceiroCliente cliente) ++ "\n" ++
                    "Tipo de automóvel: " ++ tipoVeiculo (veiculoCliente cliente) ++ "\n"

        -- Detalhes do histórico de sinistros
        infoSinistros = "Sinistros Registrados: " ++ show (numSinistrosCliente cliente) ++ "\n" ++
                        "Detalhes dos Sinistros: \n" ++ intercalate "\n" (map formatarSinistro sinistros)
    in
        -- Combinando as duas partes do relatório.
        infoCliente ++ "\n" ++ infoNivel ++ "\n" ++ infoSinistros

-- Lista vazia de seguros para o cliente (corrigido)
listaSegurosCliente :: Cliente -> [Seguro]
listaSegurosCliente _ = []
