module AfastamentoTest (runAfastamentoTests) where

import Model.TiposDados
import Controller.Afastamento
import Controller.GerenciaFuncionarios (buscarFuncionario)
import Data.Time (fromGregorian, Day)

assert :: Bool -> String -> IO ()
assert condicao msg
  | condicao  = putStrLn $ "Testes PASSARAM:  " ++ msg
  | otherwise = putStrLn $ "Testes FALHARAM: " ++ msg

-- Datas de teste
inicio :: Day
inicio = fromGregorian 2025 1 10

fim :: Day
fim = fromGregorian 2025 1 20

fimInvalido :: Day
fimInvalido = fromGregorian 2024 12 31

-- Funcionários
funcAtivo :: Funcionario
funcAtivo =
  Funcionario "11111111111" "Carlos" inicio "M" "c@x.com" "9999" "PB"
    Ativo "link" 1 1 [] 40 inicio

funcDesligado :: Funcionario
funcDesligado =
  Funcionario "22222222222" "Ana" inicio "F" "a@x.com" "8888" "PE"
    Desligado "link" 1 1 [] 40 inicio

-- Afastamentos
afastamentoValido :: Afastamento
afastamentoValido =
  Afastamento 1 "11111111111" AfastamentoMedico inicio fim
    "Cirurgia" Nothing

afastamentoConflitante :: Afastamento
afastamentoConflitante =
  Afastamento 2 "11111111111" LicencaMaternidade inicio fim
    "Outro motivo" Nothing

afastamentoPeriodoInvalido :: Afastamento
afastamentoPeriodoInvalido =
  Afastamento 3 "11111111111" AfastamentoMedico inicio fimInvalido
    "Erro de data" Nothing

-- TESTES ------------------------------------------------------------

registrarAfastamentoValidoTeste :: IO ()
registrarAfastamentoValidoTeste = do
  let funcionarios = [funcAtivo]
  let afastamentos = []

  case registrarAfastamento afastamentoValido funcionarios afastamentos of
    Left _ -> assert False "Falhou ao registrar afastamento válido."
    Right (funcs, afs) -> do
      let Just f = buscarFuncionario "11111111111" funcs
      assert (statusFunc f == Afastado)
        "Funcionário ficou com status Afastado."
      assert (length afs == 1)
        "Afastamento foi adicionado à lista."

funcionarioInexistenteTeste :: IO ()
funcionarioInexistenteTeste = do
  let funcionarios = []
  let afastamentos = []

  case registrarAfastamento afastamentoValido funcionarios afastamentos of
    Left _ -> assert True "Bloqueou funcionário inexistente."
    Right _ -> assert False "Erro: permitiu afastamento sem funcionário."

funcionarioNaoAtivoTeste :: IO ()
funcionarioNaoAtivoTeste = do
  let funcionarios = [funcDesligado]
  let afastamentos = []

  case registrarAfastamento afastamentoValido funcionarios afastamentos of
    Left _ -> assert True "Bloqueou afastamento de funcionário não ativo."
    Right _ -> assert False "Erro: afastou funcionário desligado."

periodoInvalidoTeste :: IO ()
periodoInvalidoTeste = do
  let funcionarios = [funcAtivo]
  let afastamentos = []

  case registrarAfastamento afastamentoPeriodoInvalido funcionarios afastamentos of
    Left _ -> assert True "Bloqueou período inválido."
    Right _ -> assert False "Erro: aceitou período inválido."

conflitoAfastamentoTeste :: IO ()
conflitoAfastamentoTeste = do
  let funcionarios = [funcAtivo]
  let afastamentos = [afastamentoValido]

  case registrarAfastamento afastamentoConflitante funcionarios afastamentos of
    Left _ -> assert True "Bloqueou afastamento conflitante."
    Right _ -> assert False "Erro: permitiu conflito de afastamentos."

-- RUNNER ------------------------------------------------------------

runAfastamentoTests :: IO ()
runAfastamentoTests = do
  putStrLn "\n--- Testes de Afastamento ---"

  registrarAfastamentoValidoTeste
  funcionarioInexistenteTeste
  funcionarioNaoAtivoTeste
  periodoInvalidoTeste
  conflitoAfastamentoTeste
