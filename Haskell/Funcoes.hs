module Funcoes where

-- vai pegar todos os atributos necessarios e retornar um inteiro de 1 a 5
calcularNivelCliente :: Int -> String -> Int --esse input Int e String eh um exemplo
calcularNivelCliente = 3

--vai receber o custo do sinistro e um inteiro do nivel do cliente e retorna o valor do desconto
calcularDesconto :: Float -> Int -> Float
calculaDesconto custoSinistro nivelCliente
    | nivelCliente = 1 = custoSinistro
    | nivelCliente = 2 = custoSinistro * 0.1
    | nivelCliente = 3 = custoSinistro * 0.15
    | nivelCliente = 4 = custoSinistro * 0.2
    | nivelCliente = 5 = custoSinistro * 0.25
    | otherwise = error "Nivel de Cliente invalido!"

--vai receber o custo do sinistro e um inteiro do nivel do cliente e retorna o valor do desconto
calcularValorDescontado :: Float -> Int -> Float
calcularValorDescontado custoSinistro nivelCliente
    | nivelCliente = 1 = custoSinistro
    | nivelCliente = 2 = custoSinistro - (custoSinistro * 0.1)
    | nivelCliente = 3 = custoSinistro - (custoSinistro * 0.15)
    | nivelCliente = 4 = custoSinistro - (custoSinistro * 0.2)
    | nivelCliente = 5 = custoSinistro - (custoSinistro * 0.25)
    | otherwise = error "Nivel de Cliente invalido!"

--limpar a tela
limparTela :: String
limparTela = ""

--simula o seguro baseado em atributos do cliente
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
    | estadoCivil == "Solteiro" = 1.025  -- Solteiro - 2.5% mais caro
    | estadoCivil == "Casado"   = 1.00   -- Casado - nenhum ajuste
    | otherwise                 = 1.00   -- Nenhum ajuste para outros casos

--Funcao auxiliar para calcular o ajuste de risco baseado no sexo
sexoRisco :: String -> Float
sexoRisco sexo
    | sexo == "Homem"  = 1.05  -- Homem - 5% mais caro
    | sexo == "Mulher" = 1.00  -- Mulher - nenhum ajuste
    | sexo == "H"  = 1.05  -- Homem - 5% mais caro
    | sexo == "M" = 1.00  -- Mulher - nenhum ajuste
    | otherwise        = 1.00  -- Nenhum ajuste para outros casos