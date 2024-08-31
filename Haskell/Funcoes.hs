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