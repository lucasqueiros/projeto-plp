-- Modulo contendo todas as funções relativas ao cliente
module Cliente where


import Data.Time
import System.IO
import Utils

-- Função auxiliar para cadastro de cliente
cadastrarClienteMain :: IO ()
cadastrarClienteMain = do
    cpf <-Utils.solicitarCpf
    clienteExistente <- Utils.buscarCpfRegistrado cpf

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
            idade <- Utils.calcularIdade dataNascimento
            putStrLn "Digite o modelo do automóvel:"
            modelo <- getLine
            putStrLn "Digite o ano do automóvel:"
            anoStr <- getLine
            let ano = read anoStr :: Int
            putStrLn "Digite o tipo do veículo (Carro ou Moto):"
            tipoVeiculo <- getLine
            placa <- Utils.solicitarPlacaMercosul
            placaExistente <- buscarPlacaRegistrada placa

            case placaExistente of
                Just _ -> putStrLn "Placa já registrada. Cadastramento encerrado."
                Nothing -> do
                    let nivelCliente = 2
                    let (cliente, automovel) = cadastrarCliente nome cpf telefone sexo idade modelo ano placa tipoVeiculo nivelCliente
                    salvarCliente cliente automovel
                    putStrLn "\nCliente cadastrado com sucesso!"

-- Função para editar um cliente existente
editarClienteMain :: IO ()
editarClienteMain = do
    cpf <- Utils.solicitarCpf
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
                    novoTelefone <- Utils.solicitarTelefone
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
    let nomeClienteFormatado = take 22 $ nomeCliente cliente ++ replicate 22 ' '
        modeloCarroFormatado = take 18 $ modeloVeiculo automovel ++ replicate 18 ' '
        placaCarroFormatado = take 15 $ placaVeiculo automovel ++ replicate 15 ' '
        nivelClienteStrFormatado = take 10 $ show (nivelCliente cliente) ++ replicate 10 ' '
    in "| " ++ nomeClienteFormatado ++ " | " ++ modeloCarroFormatado ++ " | " ++ placaCarroFormatado ++ " | " ++ nivelClienteStrFormatado ++ " |"

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
automovelToString (Veiculo modelo ano placa tipoVeiculo) =
    "Veiculo {modelo = \"" ++ modelo ++ "\", ano = " ++ show ano ++ ", placa = \"" ++ placa ++ "\", tipoVeiculo = \"" ++ tipoVeiculo ++ "\"}"

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

-- Lista vazia de seguros para o cliente (corrigido)
listaSegurosCliente :: Cliente -> [Seguro]
listaSegurosCliente _ = []

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

