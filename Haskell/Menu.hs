module Menu (menu) where

import Funcoes

--Modulo com todos os menus de interação com o usuario

--Menu Principal
menu :: IO ()
menu = do
    putStrLn "Escolha uma opção:"
    putStrLn "1. Administrador"
    putStrLn "2. Cliente"
    putStrLn "0. Sair"
    opcao <- getLine
    case opcao of
        "1" -> menuAdministrador
        "2" -> menuCliente
        "0" -> putStrLn "Saindo..."
        _   -> do
            putStrLn "Opção inválida!"
            menu

--Menu do administrador
menuAdministrador :: IO ()
menuAdministrador = do
    putStrLn "Opções do Administrador:"
    putStrLn "1. Gerenciar Clientes" 
    putStrLn "2. Gerenciar Contratos de Seguros" 
    putStrLn "3. Gerenciar Sinistros" 
    putStrLn "0. Voltar"
    opcao <- getLine
    case opcao of
        "1" -> crudCliente
        "2" -> crudSeguro
        "3" -> crudSinistro
        "0" -> menu
        _   -> do
            putStrLn "Opção inválida!"
            menuAdministrador

-- Menu do cliente
menuCliente :: IO ()
menuCliente = do
    putStrLn "Opções do Cliente:"
    putStrLn "1. Gerar Relatorio"
    putStrLn "2. Status Pagamento"
    putStrLn "0. Voltar"
    opcao <- getLine
    case opcao of
        "1" -> putStrLn "teste"
        "2" -> putStrLn "teste"
        "0" -> menu
        _   -> do
            putStrLn "Opção inválida!"
            menuCliente

--Menu do gerenciamento de clientes
crudCliente :: IO()
crudCliente = do
    putStrLn "Gerenciamento de Clientes"
    putStrLn "1. Cadastrar Cliente"
    putStrLn "2. Alterar Cliente"
    putStrLn "3. Listar Clientes"
    putStrLn "4. Buscar Cliente"
    putStrLn "0. Voltar"
    opcao <- getLine
    case opcao of
        "1" -> Funcoes.cadastrarClienteMain --chama funcao
        "2" -> Funcoes.editarClienteMain --chama funcao
        "3" -> Funcoes.listarClientes --chama funcao
        "4" -> putStrLn "teste" --chama funcao
        "0" -> menu

--Menu do gerenciamento de seguros
crudSeguro :: IO()
crudSeguro = do
    putStrLn "Gerenciamento de Seguros"
    putStrLn "1. Criar Contrato"
    putStrLn "2. Listar Contratos Ativos"
    putStrLn "3. Buscar Contratos do Cliente"
    putStrLn "0. Voltar"
    opcao <- getLine
    case opcao of
        "1" -> putStrLn "teste" --chama funcao
        "2" -> putStrLn "teste" --chama funcao
        "3" -> putStrLn "teste" --chama funcao
        "0" -> menu

--Menu de gerenciamentos de sinistros
crudSinistro :: IO()
crudSinistro = do
    putStrLn "Gerenciamento de Sinistros"
    putStrLn "1. Adicionar Sinistro"
    putStrLn "2. Encerrar Sinistro"
    putStrLn "3. Listar Sinistros Ativos"
    putStrLn "4. Pericia do Sinistro"
    putStrLn "0. Voltar"
    opcao <- getLine
    case opcao of
        "1" -> putStrLn "teste" --chama funcao
        "2" -> putStrLn "teste" --chama funcao
        "3" -> putStrLn "teste" --chama funcao
        "4" -> putStrLn "teste" --chama funcao
        "0" -> menu