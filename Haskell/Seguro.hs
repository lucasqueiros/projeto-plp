module Seguro where

import Tipos
import Utils

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

-- Função que Verifica o Status Financeiro do Seguro
pagamentoEmDia :: Bool -> String 
pagamentoEmDia status = 
    if status
        then "Em dia!"
    else "Devendo!"

    