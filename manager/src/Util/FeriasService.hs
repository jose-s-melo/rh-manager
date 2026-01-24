module Util.FeriasService where

import Controller.GerenciaFerias
import Model.TiposDados
import Util.Utilitarios
import Data.Time
import qualified Data.Map as Map

-- SISTEMA

iniciarSistemaFerias :: IO ()
iniciarSistemaFerias = loop (GerenciadorFerias Map.empty)

loop :: GerenciadorFerias -> IO ()
loop ger = do
  cabecalho "MENU - SISTEMA DE FÉRIAS"
  putStrLn "1 - Registrar férias"
  putStrLn "2 - Consultar férias"
  putStrLn "3 - Verificar direito a férias"
  putStrLn "0 - Sair"
  putStrLn ""

  opcao <- lerLinha "Selecione uma opção"

  case opcao of
    "1" -> registrarFeriasUI ger >>= finalizar
    "2" -> exibirFeriasUI ger >>= finalizar
    "3" -> verificarDireitoUI ger >> pause >> loop ger
    "0" -> putStrLn "\nSistema encerrado."
    _   -> msgErro "Opção inválida." >> putStrLn "" >> pause >> loop ger
  where
    finalizar g = pause >> loop g

admitirFuncionarioService
  :: CPF -> Day -> GerenciadorFerias -> Either String GerenciadorFerias
admitirFuncionarioService = admitirFuncionario

registrarFaltaService
  :: CPF -> Day -> GerenciadorFerias -> Either String GerenciadorFerias
registrarFaltaService cpf dia ger = do
  reg <-
    maybe
      (Left "Funcionário não encontrado.")
      Right
      (Map.lookup cpf (registros ger))

  regAtualizado <- registrarFalta dia reg

  Right ger {
    registros = Map.insert cpf regAtualizado (registros ger)
  }

verificarDireitoService
  :: CPF -> Day -> GerenciadorFerias -> Either String [CicloFerias]
verificarDireitoService cpf ref ger = do
  reg <-
    maybe
      (Left "Funcionário não encontrado.")
      Right
      (Map.lookup cpf (registros ger))

  let ciclosAtualizados =
        map (atualizarCiclo ref)
          (takeWhile
            (\c -> inicioAquisitivo (periodoAquisitivo c) <= ref)
            (ciclos reg))

      ciclosComDireito =
        filter (temDireitoAFerias ref) ciclosAtualizados

  Right ciclosComDireito

registrarFeriasUI :: GerenciadorFerias -> IO GerenciadorFerias
registrarFeriasUI ger = do
  cabecalho "REGISTRO DE FÉRIAS"
  cpf <- lerCPF
  fracionada <- lerSN "Férias fracionadas?"

  qtd <- if fracionada then lerQuantidadePeriodos else return 1
  periodos <- mapM lerPeriodo [1 .. qtd]

  ref <- lerData "Data de referência para validação"

  confirmar "Confirmar registro das férias?" >>= \ok ->
    if not ok
      then msgInfo "Registro cancelado." >> putStrLn "" >> return ger
      else registrarSequencial cpf periodos ref ger

lerQuantidadePeriodos :: IO Int
lerQuantidadePeriodos = do
  n <- lerInt "Quantidade de períodos (2 ou 3)"
  if n < 2 || n > 3
    then msgErro "Quantidade inválida." >> putStrLn "" >> lerQuantidadePeriodos
    else return n

lerPeriodo :: Int -> IO (Day, Day)
lerPeriodo i = do
  putStrLn ""
  putStrLn ("Período " ++ show i)
  ini <- lerData "Data de início"
  fim <- lerData "Data de fim"
  return (ini, fim)

exibirFeriasUI :: GerenciadorFerias -> IO GerenciadorFerias
exibirFeriasUI ger = do
  cabecalho "CONSULTA DE FÉRIAS"
  cpf <- lerCPF
  ref <- lerData "Data de referência"

  case Map.lookup cpf (registros ger) of
    Nothing -> msgErro "Funcionário não encontrado." >> putStrLn ""
    Just reg -> do
      let ciclosAtualizados =
            map (atualizarCiclo ref)
              (takeWhile
                (\c -> inicioAquisitivo (periodoAquisitivo c) <= ref)
                (ciclos reg))

          ciclosExibidos =
            filter
              (\c ->
                statusCiclo c /= Esperado ||
                any (\f ->
                      statusFerias f == EmAndamento ||
                      statusFerias f == Concluida)
                    (feriasDoCiclo c))
              ciclosAtualizados

      if null ciclosExibidos
        then msgInfo "Nenhum ciclo relevante encontrado." >> putStrLn ""
        else mapM_ exibirFeriasPersonalizado ciclosExibidos

  return ger

verificarDireitoUI :: GerenciadorFerias -> IO ()
verificarDireitoUI ger = do
  cabecalho "VERIFICAÇÃO DE DIREITO A FÉRIAS"
  cpf <- lerCPF
  ref <- lerData "Data de referência"

  case verificarDireitoService cpf ref ger of
    Left err -> msgErro err >> putStrLn ""
    Right [] -> msgInfo "Funcionário ainda não possui direito a férias." >> putStrLn ""
    Right cs -> do
      msgSucesso "Funcionário possui direito a férias."
      mapM_ exibirResumoCiclo cs

exibirResumoCiclo :: CicloFerias -> IO ()
exibirResumoCiclo c = do
  let pa = periodoAquisitivo c
  putStrLn ""
  putStrLn ("Período aquisitivo : "
    ++ show (inicioAquisitivo pa)
    ++ " / "
    ++ show (fimAquisitivo pa))
  putStrLn ("Saldo disponível  : " ++ show (saldoDias c) ++ " dias")

exibirFeriasPersonalizado :: CicloFerias -> IO ()
exibirFeriasPersonalizado c = do
  putStrLn ""
  putStrLn "----------------------------------------"
  putStrLn ("Ciclo aquisitivo : " ++ show iniAq ++ " / " ++ show fimAq)
  putStrLn ("Ciclo concessivo : " ++ show iniCon ++ " / " ++ show fimCon)
  putStrLn ("Status do ciclo  : " ++ show (statusCiclo c))
  putStrLn ("Saldo disponível : " ++ show (saldoDias c) ++ " dias")
  putStrLn ""
  putStrLn "Férias registradas:"

  if null (feriasDoCiclo c)
    then putStrLn "  Nenhuma férias registrada neste ciclo."
    else mapM_ exibirFeriasDetalhada (feriasDoCiclo c)
  where
    pa = periodoAquisitivo c
    pc = periodoConcessivo c
    iniAq = inicioAquisitivo pa
    fimAq = fimAquisitivo pa
    iniCon = inicioConcessivo pc
    fimCon = fimConcessivo pc

exibirFeriasDetalhada :: Ferias -> IO ()
exibirFeriasDetalhada f =
  putStrLn $
    "  Período: " ++
    show (inicioFerias f) ++ " até " ++
    show (fimFerias f) ++
    " (" ++ show (diasUtilizados f) ++ " dias) - " ++
    show (statusFerias f)

criarFerias :: Day -> Day -> Ferias
criarFerias ini fim =
  Ferias ini fim (fromIntegral (diffDays fim ini) + 1) Planejada

registrarSequencial
  :: CPF -> [(Day, Day)] -> Day -> GerenciadorFerias -> IO GerenciadorFerias
registrarSequencial _ [] _ ger =
  msgSucesso "Férias registradas com sucesso." >> putStrLn "" >> return ger

registrarSequencial cpf ((ini, fim):xs) ref ger = do
  let ferias = criarFerias ini fim
  case registrarFerias cpf ferias ref ger of
    Left err -> msgErro err >> putStrLn "" >> return ger
    Right g  -> registrarSequencial cpf xs ref g
