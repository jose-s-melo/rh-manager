module JornadaLicencaSpec (runLicenseTests) where

import Data.Time (fromGregorian, Day)
import Model.TiposDados
import Controller.JornadaLicenca

assert :: Bool -> String -> IO ()
assert condicao msg
  | condicao  = putStrLn $ "Testes PASSARAM:  " ++ msg
  | otherwise = putStrLn $ "Testes FALHARAM: " ++ msg

--
-- SETUP PARA OS TESTES DE LICENÇA
--

inicio1, fimValido, fimInvalido, fimInvertido :: Day
inicio1      = fromGregorian 2024 6 1
fimValido    = fromGregorian 2024 6 4
fimInvalido  = fromGregorian 2024 6 10
fimInvertido = fromGregorian 2024 5 30


licencaCasamentoValida :: Licenca
licencaCasamentoValida = Licenca
  { tipoLicensa = Casamento
  , dataInicio  = inicio1
  , dataFim     = fimValido
  , descricao   = "Casamento dentro do prazo"
  }

licencaCasamentoInvalida :: Licenca
licencaCasamentoInvalida = Licenca
  { tipoLicensa = Casamento
  , dataInicio  = inicio1
  , dataFim     = fimInvalido
  , descricao   = "Casamento fora do prazo"
  }

licencaDatasInvertidas :: Licenca
licencaDatasInvertidas = Licenca
  { tipoLicensa = Luto
  , dataInicio  = inicio1
  , dataFim     = fimInvertido
  , descricao   = "Datas invertidas"
  }


licencaValidaTeste :: IO ()
licencaValidaTeste = do
  assert (verificarSeLicencaValida licencaCasamentoValida)
    "Licença de casamento válida foi aceita."

licencaInvalidaPorDiasTeste :: IO ()
licencaInvalidaPorDiasTeste = do
  assert (not (verificarSeLicencaValida licencaCasamentoInvalida))
    "Licença de casamento fora do prazo foi rejeitada."

licencaDatasInvertidasTeste :: IO ()
licencaDatasInvertidasTeste = do
  assert (not (verificarSeLicencaValida licencaDatasInvertidas))
    "Licença com datas invertidas foi rejeitada."

--
-- SETUP PARA OS TESTES DE CICLO DE FOLGA
--

ultimaFolgaData, dataFolgaValida, dataFolgaInvalida :: Day
ultimaFolgaData = fromGregorian 2024 6 1
dataFolgaValida = fromGregorian 2024 6 5   
dataFolgaInvalida = fromGregorian 2024 6 10

cicloFolgaValido :: CicloFolga
cicloFolgaValido = Folga
  { ultimaFolga = ultimaFolgaData
  , dataFolga = dataFolgaValida
  }

cicloFolgaInvalido :: CicloFolga
cicloFolgaInvalido = Folga
  { ultimaFolga = ultimaFolgaData
  , dataFolga = dataFolgaInvalida
  }

cicloFolgaMesmoDia :: CicloFolga
cicloFolgaMesmoDia = Folga
  { ultimaFolga = ultimaFolgaData
  , dataFolga = ultimaFolgaData
  }

cicloFolgaDatasInvertidas :: CicloFolga
cicloFolgaDatasInvertidas = Folga
  { ultimaFolga = fromGregorian 2024 6 10
  , dataFolga = fromGregorian 2024 6 1
  }


verificaLegalidadeCicloValidoTeste :: IO ()
verificaLegalidadeCicloValidoTeste = do
  assert (verificaLegalidadeDeCicloFolga cicloFolgaValido)
    "Ciclo de folga válido (4 dias) foi aceito."

verificaLegalidadeCicloInvalidoTeste :: IO ()
verificaLegalidadeCicloInvalidoTeste = do
  assert (not (verificaLegalidadeDeCicloFolga cicloFolgaInvalido))
    "Ciclo de folga inválido (9 dias >= 7) foi rejeitado."

verificaLegalidadeCicloMesmoDiaTeste :: IO ()
verificaLegalidadeCicloMesmoDiaTeste = do
  assert (verificaLegalidadeDeCicloFolga cicloFolgaMesmoDia)
    "Ciclo de folga com mesmo dia (0 dias) foi aceito."

verificaLegalidadeCicloDatasInvertidosTeste :: IO ()
verificaLegalidadeCicloDatasInvertidosTeste = do
  assert (not (verificaLegalidadeDeCicloFolga cicloFolgaDatasInvertidas))
    "Ciclo de folga com datas invertidas foi rejeitado."


buscaDiaDeFolgaComDiasFaltantesTeste :: IO ()
buscaDiaDeFolgaComDiasFaltantesTeste = do
  let resultado = buscaDiaDeFolga cicloFolgaValido
  assert (resultado == Just dataFolgaValida)
    "Busca de dia de folga com dias faltantes retornou a data corretamente."

--
-- SETUP PARA TESTES DE ESCALA SEMANAL E JORNADA DIÁRIA
-- 

jornadaValida4h :: JornadaDiaria
jornadaValida4h = JornadaDiaria
  { inicio = 8
  , fim = 12
  }

jornadaValida8h :: JornadaDiaria
jornadaValida8h = JornadaDiaria
  { inicio = 8
  , fim = 16
  }

jornadaInvalidaInvertida :: JornadaDiaria
jornadaInvalidaInvertida = JornadaDiaria
  { inicio = 14
  , fim = 10
  }

jornadaInvalidaExcesso :: JornadaDiaria
jornadaInvalidaExcesso = JornadaDiaria
  { inicio = 8
  , fim = 18
  }

diasSemana :: [Day]
diasSemana =
  [ fromGregorian 2024 6 3
  , fromGregorian 2024 6 4
  , fromGregorian 2024 6 5
  , fromGregorian 2024 6 6
  , fromGregorian 2024 6 7
  ]

escalaSemanalValida40h :: EscalaSemanal
escalaSemanalValida40h = EscalaSemanal
  { idFuncionarioSemanal = "12345678900"
  , diasTrabalho = diasSemana
  , jornadas = replicate 5 jornadaValida8h
  }

escalaSemanalInvalidaHoras :: EscalaSemanal
escalaSemanalInvalidaHoras = EscalaSemanal
  { idFuncionarioSemanal = "12345678900"
  , diasTrabalho = diasSemana
  , jornadas = replicate 5 jornadaInvalidaExcesso
  }

escalaSemanalEstruturaInvalida :: EscalaSemanal
escalaSemanalEstruturaInvalida = EscalaSemanal
  { idFuncionarioSemanal = "12345678900"
  , diasTrabalho = diasSemana
  , jornadas = [jornadaValida8h] -- tamanhos diferentes
  }



calculaHorasPorDiaTeste :: IO ()
calculaHorasPorDiaTeste = do
  assert (calculaHorasTrabalhadasPorDia jornadaValida4h == 4)
    "Cálculo de horas diárias (4h) funcionou corretamente."

calculaHorasPorDia8hTeste :: IO ()
calculaHorasPorDia8hTeste = do
  assert (calculaHorasTrabalhadasPorDia jornadaValida8h == 8)
    "Cálculo de horas diárias (8h) funcionou corretamente."

calculaHorasSemanaTeste :: IO ()
calculaHorasSemanaTeste = do
  assert (calculaHorasTrabalhadasPorSemana escalaSemanalValida40h == 40)
    "Cálculo de horas semanais (40h) funcionou corretamente."
  
verificaJornadaValidaTeste :: IO ()
verificaJornadaValidaTeste = do
  assert (verificaJornadaValida jornadaValida8h)
    "Jornada válida foi aceita."

verificaJornadaInvalidaTeste :: IO ()
verificaJornadaInvalidaTeste = do
  assert (not (verificaJornadaValida jornadaInvalidaInvertida))
    "Jornada com horários invertidos foi rejeitada."

verificaEscalaValidaTeste :: IO ()
verificaEscalaValidaTeste = do
  assert (verificaEscalaValida escalaSemanalValida40h)
    "Escala semanal válida foi aceita."

verificaEscalaEstruturaInvalidaTeste :: IO ()
verificaEscalaEstruturaInvalidaTeste = do
  assert (not (verificaEscalaValida escalaSemanalEstruturaInvalida))
    "Escala com dias e jornadas inconsistentes foi rejeitada."


runLicenseTests :: IO ()
runLicenseTests = do
  putStrLn "\n--- Testes de Licença ---"

  licencaValidaTeste
  licencaInvalidaPorDiasTeste
  licencaDatasInvertidasTeste

  putStrLn "\n--- Testes de Ciclo de Folga ---"

  verificaLegalidadeCicloValidoTeste
  verificaLegalidadeCicloInvalidoTeste
  verificaLegalidadeCicloMesmoDiaTeste
  verificaLegalidadeCicloDatasInvertidosTeste
  buscaDiaDeFolgaComDiasFaltantesTeste

  putStrLn "\n--- Testes de Jornada Diaria ---"

  calculaHorasPorDiaTeste
  calculaHorasPorDia8hTeste
  calculaHorasSemanaTeste
  verificaJornadaValidaTeste
  verificaJornadaInvalidaTeste
  verificaEscalaValidaTeste
  verificaEscalaEstruturaInvalidaTeste