module Sinistro where

import Tipos
import Utils
import Data.Time (Day, getCurrentTime, utctDay, parseTimeOrError, defaultTimeLocale, toGregorian)


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
    let sinistros = map Utils.parseSinistro (lines conteudo)
        sinistrosAtualizados = map (\s -> if idSinistro s == idS then s {ativoSinistro = False} else s) sinistros
    writeFile "dados/sinistros.txt" (unlines (map show sinistrosAtualizados))
    putStrLn "Sinistro encerrado com sucesso."

--listarSinistro
listarSinistrosAtivos :: IO ()
listarSinistrosAtivos = do
    conteudo <- readFile "dados/sinistros.txt"
    let sinistros = map Utils.parseSinistro (lines conteudo) :: [Sinistro]
        sinistrosAtivos = filter ativoSinistro sinistros
    mapM_ (putStrLn . show) sinistrosAtivos
