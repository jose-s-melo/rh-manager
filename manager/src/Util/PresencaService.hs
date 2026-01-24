module Util.PresencaService where

import Controller.ControladorPresenca
import Model.TiposDados
import Util.Utilitarios
import Data.Time
import qualified Data.Map as Map

-------------------------------------------------
-- SISTEMA
-------------------------------------------------

iniciarSistemaPresenca :: IO ()
iniciarSistemaPresenca = loop (SistemaDePresenca Map.empty)

loop :: SistemaDePresenca -> IO ()
loop sistema = do
  cabecalho "MENU - SISTEMA DE PRESENÇA"
  putStrLn "1 - Registrar presença"
  putStrLn "2 - Consultar presença por dia"
  putStrLn "3 - Exibir histórico de presenças"
  putStrLn "4 - Calcular horas trabalhadas"
  putStrLn "5 - Calcular faltas injustificadas"
  putStrLn "0 - Sair"
  putStrLn ""

  opcao <- lerLinha "Selecione uma opção"

  case opcao of
    "1" -> registrarPresencaUI sistema >>= finalizar
    "2" -> consultarPresencaUI sistema >> pause >> loop sistema
    "3" -> exibirHistoricoUI sistema >> pause >> loop sistema
    "4" -> calcularHorasUI sistema >> pause >> loop sistema
    "5" -> calcularFaltasUI sistema >> pause >> loop sistema
    "0" -> putStrLn "\nSistema encerrado."
    _   -> msgErro "Opção inválida." >> pause >> loop sistema
  where
    finalizar s = pause >> loop s

-------------------------------------------------
-- UI
-------------------------------------------------

registrarPresencaUI :: SistemaDePresenca -> IO SistemaDePresenca
registrarPresencaUI sistema = do
  cabecalho "REGISTRO DE PRESENÇA"

  cpf <- lerCPF
  dia <- lerData "Data da presença"
  deptoId <- lerInt "ID do departamento"
  compareceuFlag <- lerSN "Funcionário compareceu?"

  presenca <-
    if compareceuFlag
      then do
        modalidade <- lerModalidade
        entrada <- lerHora "Horário de entrada"
        saida <- lerHora "Horário de saída"
        return $
          Presenca
            { idDepartamento = deptoId
            , tipoPresenca = modalidade
            , checkIn = entrada
            , checkOut = saida
            , compareceu = True
            , justificativa = ""
            }
      else do
        just <- lerLinha "Justificativa (vazio para injustificada)"
        return $
          Presenca
            { idDepartamento = deptoId
            , tipoPresenca = Presencial
            , checkIn = midnight
            , checkOut = midnight
            , compareceu = False
            , justificativa = just
            }

  case registrarPresenca cpf dia presenca sistema of
    Left err -> msgErro err >> return sistema
    Right s  -> msgSucesso "Presença registrada com sucesso." >> return s

consultarPresencaUI :: SistemaDePresenca -> IO ()
consultarPresencaUI sistema = do
  cabecalho "CONSULTA DE PRESENÇA"
  cpf <- lerCPF
  dia <- lerData "Data a consultar"

  case buscarPresenca cpf dia sistema of
    Nothing -> msgInfo "Nenhuma presença registrada para este dia."
    Just p  -> exibirPresenca (dia, p)

exibirHistoricoUI :: SistemaDePresenca -> IO ()
exibirHistoricoUI sistema = do
  cabecalho "HISTÓRICO DE PRESENÇAS"
  cpf <- lerCPF
  exibirRegistroPresencas cpf sistema

calcularHorasUI :: SistemaDePresenca -> IO ()
calcularHorasUI sistema = do
  cabecalho "CÁLCULO DE HORAS TRABALHADAS"
  cpf <- lerCPF
  ini <- lerData "Data inicial"
  fim <- lerData "Data final"

  let total = horasTrabalhadas cpf ini fim sistema
  putStrLn ("\nTotal de horas trabalhadas: " ++ show total)

calcularFaltasUI :: SistemaDePresenca -> IO ()
calcularFaltasUI sistema = do
  cabecalho "FALTAS INJUSTIFICADAS"
  cpf <- lerCPF
  ini <- lerData "Data inicial"
  fim <- lerData "Data final"

  let qtd = faltasInjustificadas cpf ini fim sistema
  putStrLn ("\nFaltas injustificadas no período: " ++ show qtd)

-------------------------------------------------
-- FORMATAÇÃO / EXIBIÇÃO
-------------------------------------------------

formatarPresenca :: (Day, Presenca) -> String
formatarPresenca (dia, p)
  | compareceu p =
      show dia
        ++ " | COMPARECEU"
        ++ " | " ++ show (tipoPresenca p)
        ++ " | " ++ show (checkIn p)
        ++ " - " ++ show (checkOut p)
        ++ " | Depto: " ++ show (idDepartamento p)
  | otherwise =
      show dia
        ++ " |  AUSENTE  "
        ++ statusJustificativa
  where
    statusJustificativa =
      if null (justificativa p)
        then " | INJUSTIFICADA"
        else " | JUSTIFICADA: " ++ justificativa p

exibirPresenca :: (Day, Presenca) -> IO ()
exibirPresenca = putStrLn . formatarPresenca

exibirRegistroPresencas :: CPF -> SistemaDePresenca -> IO ()
exibirRegistroPresencas cpf sistema =
  case Map.lookup cpf (presencasRegistradas sistema) of
    Nothing ->
      putStrLn "Nenhum registro de presença encontrado para este CPF."
    Just mapaDias -> do
      putStrLn "\nDATA       |   STATUS   | DETALHES"
      putStrLn "-----------------------------------------------"
      mapM_ exibirPresenca (Map.toAscList mapaDias)

-------------------------------------------------
-- LEITURAS AUXILIARES
-------------------------------------------------

lerModalidade :: IO Modalidade
lerModalidade = do
  putStrLn "Modalidade:"
  putStrLn "1 - Presencial"
  putStrLn "2 - Remoto"
  op <- lerLinha "Escolha"

  case op of
    "1" -> return Presencial
    "2" -> return Remoto
    _   -> msgErro "Opção inválida." >> lerModalidade

lerHora :: String -> IO TimeOfDay
lerHora msg = do
  putStrLn msg
  h <- lerInt "Hora (0-23)"
  m <- lerInt "Minuto (0-59)"

  if h < 0 || h > 23 || m < 0 || m > 59
    then msgErro "Horário inválido." >> lerHora msg
    else return (TimeOfDay h m 0)
