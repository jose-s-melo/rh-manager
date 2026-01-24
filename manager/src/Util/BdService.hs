module Util.BdService where

import Controller.GerenciaFuncionarios
import Controller.GerenciaDepartamentos
import Controller.GerenciaCargos
import Controller.Afastamento
import Model.TiposDados
import Util.Utilitarios
import Data.Time
import qualified Data.Map as Map

-------------------------------------------------
-- SISTEMA PRINCIPAL
-------------------------------------------------

iniciarSistemaBD :: IO ()
iniciarSistemaBD =
  loop (SistemaBancoDadosRH [] [] [] [])

loop :: SistemaBancoDadosRH -> IO ()
loop sistema = do
  cabecalho "MENU - SISTEMA DE BANCO DE DADOS RH"

  putStrLn "1 - Gerenciar funcionários"
  putStrLn "2 - Gerenciar departamentos"
  putStrLn "3 - Gerenciar cargos"
  putStrLn "0 - Sair"

  op <- lerLinha "Selecione uma opção"

  case op of
    "1" -> menuFuncionariosLoop sistema >>= loop
    "2" -> menuDepartamentosLoop sistema >>= loop
    "3" -> menuCargosLoop sistema >>= loop
    "0" -> putStrLn "\nSistema encerrado."
    _   -> msgErro "Opção inválida." >> pause >> loop sistema

-------------------------------------------------
-- MENU FUNCIONÁRIOS
-------------------------------------------------

menuFuncionariosLoop :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
menuFuncionariosLoop sistema = do
  cabecalho "MENU - FUNCIONÁRIOS"

  putStrLn "1 - Cadastrar"
  putStrLn "2 - Alterar"
  putStrLn "3 - Buscar"
  putStrLn "4 - Excluir"
  putStrLn "5 - Listar"
  putStrLn "6 - Registrar afastamento"
  putStrLn "7 - Encerrar afastamento"
  putStrLn "8 - Listar funcionários afastados"
  putStrLn "9 - Desligar funcionário"
  putStrLn "10 - Listar funcionários desligados"
  putStrLn "0 - Voltar"

  op <- lerLinha "Escolha"

  case op of
    "1"  -> cadastrarFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "2"  -> alterarFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "3"  -> buscarFuncionarioUI sistema >> pause >> menuFuncionariosLoop sistema
    "4"  -> excluirFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "5"  -> listarFuncionariosUI sistema >> pause >> menuFuncionariosLoop sistema
    "6"  -> registrarAfastamentoUI sistema >>= pausaEVolta menuFuncionariosLoop
    "7"  -> encerrarAfastamentoUI sistema >>= pausaEVolta menuFuncionariosLoop
    "8"  -> listarAfastadosUI sistema >> pause >> menuFuncionariosLoop sistema
    "9"  -> desligarFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "10" -> listarDesligadosUI sistema >> pause >> menuFuncionariosLoop sistema
    "0"  -> return sistema
    _    -> msgErro "Opção inválida." >> pause >> menuFuncionariosLoop sistema

-------------------------------------------------
-- MENU DEPARTAMENTOS
-------------------------------------------------

menuDepartamentosLoop :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
menuDepartamentosLoop sistema = do
  cabecalho "MENU - DEPARTAMENTOS"

  putStrLn "1 - Cadastrar"
  putStrLn "2 - Alterar"
  putStrLn "3 - Buscar"
  putStrLn "4 - Excluir"
  putStrLn "5 - Listar"
  putStrLn "0 - Voltar"

  op <- lerLinha "Escolha"

  case op of
    "1" -> cadastrarDepartamentoUI sistema >>= pausaEVolta menuDepartamentosLoop
    "2" -> alterarDepartamentoUI sistema >>= pausaEVolta menuDepartamentosLoop
    "3" -> buscarDepartamentoUI sistema >> pause >> menuDepartamentosLoop sistema
    "4" -> excluirDepartamentoUI sistema >>= pausaEVolta menuDepartamentosLoop
    "5" -> listarDepartamentosUI sistema >> pause >> menuDepartamentosLoop sistema
    "0" -> return sistema
    _   -> msgErro "Opção inválida." >> pause >> menuDepartamentosLoop sistema

-------------------------------------------------
-- MENU CARGOS
-------------------------------------------------

menuCargosLoop :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
menuCargosLoop sistema = do
  cabecalho "MENU - CARGOS"

  putStrLn "1 - Cadastrar"
  putStrLn "2 - Alterar"
  putStrLn "3 - Buscar"
  putStrLn "4 - Excluir"
  putStrLn "5 - Listar"
  putStrLn "0 - Voltar"

  op <- lerLinha "Escolha"

  case op of
    "1" -> cadastrarCargoUI sistema >>= pausaEVolta menuCargosLoop
    "2" -> alterarCargoUI sistema >>= pausaEVolta menuCargosLoop
    "3" -> buscarCargoUI sistema >> pause >> menuCargosLoop sistema
    "4" -> excluirCargoUI sistema >>= pausaEVolta menuCargosLoop
    "5" -> listarCargosUI sistema >> pause >> menuCargosLoop sistema
    "0" -> return sistema
    _   -> msgErro "Opção inválida." >> pause >> menuCargosLoop sistema

-------------------------------------------------
-- AUXILIAR
-------------------------------------------------

pausaEVolta
  :: (SistemaBancoDadosRH -> IO SistemaBancoDadosRH)
  -> SistemaBancoDadosRH
  -> IO SistemaBancoDadosRH
pausaEVolta f sistema = pause >> f sistema

-------------------------------------------------
-- FUNCIONÁRIOS - UI
-------------------------------------------------

cadastrarFuncionarioUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
cadastrarFuncionarioUI sistema = do
  cabecalho "CADASTRAR FUNCIONÁRIO"
  funcionario <- lerFuncionario

  case adicionarFuncionario funcionario (funcionarios sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Funcionário cadastrado."
        >> return sistema { funcionarios = novaLista }

alterarFuncionarioUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
alterarFuncionarioUI sistema = do
  cabecalho "ALTERAR FUNCIONÁRIO"
  funcionario <- lerFuncionario

  case modificarFuncionario funcionario (funcionarios sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Funcionário alterado."
        >> return sistema { funcionarios = novaLista }

buscarFuncionarioUI :: SistemaBancoDadosRH -> IO ()
buscarFuncionarioUI sistema = do
  cabecalho "BUSCAR FUNCIONÁRIO"
  cpf <- lerCPF

  case buscarFuncionario cpf (funcionarios sistema) of
    Nothing -> msgInfo "Funcionário não encontrado."
    Just f  -> exibirFuncionario f

excluirFuncionarioUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
excluirFuncionarioUI sistema = do
  cabecalho "EXCLUIR FUNCIONÁRIO"
  cpf <- lerCPF

  case excluirFuncionario cpf (funcionarios sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Funcionário removido."
        >> return sistema { funcionarios = novaLista }

listarFuncionariosUI :: SistemaBancoDadosRH -> IO ()
listarFuncionariosUI sistema =
  if null (funcionarios sistema)
    then msgInfo "Nenhum funcionário cadastrado."
    else mapM_ exibirFuncionario (funcionarios sistema)

-------------------------------------------------
-- AFASTAMENTOS - UI
-------------------------------------------------

registrarAfastamentoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
registrarAfastamentoUI sistema = do
  cabecalho "REGISTRAR AFASTAMENTO"

  hoje <- utctDay <$> getCurrentTime
  cpf  <- lerCPF
  tipo <- lerTipoAfastamento
  ini  <- lerData "Data início"
  fim  <- lerData "Data fim"
  desc <- lerLinha "Descrição"

  doc <- obterDocumentacaoObrigatoria tipo

  let resultado =
        registrarAfastamento
          hoje
          cpf
          tipo
          ini
          fim
          desc
          doc
          (funcionarios sistema)
          (afastamentos sistema)

  case resultado of
    Left err ->
      msgErro err >> return sistema

    Right (funcs, afs) ->
      msgSucesso "Afastamento registrado com sucesso."
        >> return sistema
             { funcionarios = funcs
             , afastamentos = afs
             }

tipoExigeDocumentacao :: TipoAfastamento -> Bool
tipoExigeDocumentacao AfastamentoMedico   = True
tipoExigeDocumentacao AcidenteDeTrabalho  = True
tipoExigeDocumentacao AusenciaJustificada = True
tipoExigeDocumentacao _                   = False

obterDocumentacaoObrigatoria :: TipoAfastamento -> IO (Maybe Documentacao)
obterDocumentacaoObrigatoria tipo
  | tipoExigeDocumentacao tipo = do
      doc <- lerDocumentacaoOpcional
      case doc of
        Nothing ->
          msgErro "Este tipo de afastamento exige documentação."
            >> obterDocumentacaoObrigatoria tipo
        Just _ ->
          return doc
  | otherwise =
      lerDocumentacaoOpcional


encerrarAfastamentoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
encerrarAfastamentoUI sistema = do
  cabecalho "ENCERRAR AFASTAMENTO"

  hoje <- utctDay <$> getCurrentTime
  idAf <- lerInt "ID do afastamento"

  case encerrarAfastamento hoje idAf (funcionarios sistema) (afastamentos sistema) of
    Left err -> msgErro err >> return sistema
    Right (funcs, afs) ->
      msgSucesso "Afastamento encerrado."
        >> return sistema { funcionarios = funcs, afastamentos = afs }

listarAfastadosUI :: SistemaBancoDadosRH -> IO ()
listarAfastadosUI sistema =
  let afastados = filter (\f -> statusFunc f == Afastado) (funcionarios sistema)
  in if null afastados
        then msgInfo "Nenhum funcionário afastado."
        else mapM_ exibirFuncionario afastados



-------------------------------------------------
-- DESLIGAMENTO - UI
-------------------------------------------------

desligarFuncionarioUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
desligarFuncionarioUI sistema = do
  cabecalho "DESLIGAR FUNCIONÁRIO"
  cpf <- lerCPF

  case buscarFuncionario cpf (funcionarios sistema) of
    Nothing -> msgErro "Funcionário não encontrado." >> return sistema
    Just f ->
      let f' = f { statusFunc = Desligado }
      in case modificarFuncionario f' (funcionarios sistema) of
           Left err -> msgErro err >> return sistema
           Right fs ->
             msgSucesso "Funcionário desligado."
               >> return sistema { funcionarios = fs }

listarDesligadosUI :: SistemaBancoDadosRH -> IO ()
listarDesligadosUI sistema =
  let desligados = filter (\f -> statusFunc f == Desligado) (funcionarios sistema)
  in if null desligados
        then msgInfo "Nenhum funcionário desligado."
        else mapM_ exibirFuncionario desligados

-------------------------------------------------
-- DEPARTAMENTOS - UI
-------------------------------------------------

cadastrarDepartamentoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
cadastrarDepartamentoUI sistema = do
  cabecalho "CADASTRAR DEPARTAMENTO"
  depto <- lerDepartamento

  case adicionarDepartamentoValidado
         depto
         (funcionarios sistema)
         (departamento sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Departamento cadastrado."
        >> return sistema { departamento = novaLista }

alterarDepartamentoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
alterarDepartamentoUI sistema = do
  cabecalho "ALTERAR DEPARTAMENTO"
  depto <- lerDepartamento

  case modificarDepartamento depto (departamento sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Departamento alterado."
        >> return sistema { departamento = novaLista }

buscarDepartamentoUI :: SistemaBancoDadosRH -> IO ()
buscarDepartamentoUI sistema = do
  cabecalho "BUSCAR DEPARTAMENTO"
  did <- lerInt "ID do departamento"

  case buscarDepartamento did (departamento sistema) of
    Nothing -> msgInfo "Departamento não encontrado."
    Just d  -> exibirDepartamento d

excluirDepartamentoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
excluirDepartamentoUI sistema = do
  cabecalho "EXCLUIR DEPARTAMENTO"
  did <- lerInt "ID do departamento"

  case excluirDepartamento did (departamento sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Departamento removido."
        >> return sistema { departamento = novaLista }

listarDepartamentosUI :: SistemaBancoDadosRH -> IO ()
listarDepartamentosUI sistema =
  if null (departamento sistema)
    then msgInfo "Nenhum departamento cadastrado."
    else mapM_ exibirDepartamento (departamento sistema)

-------------------------------------------------
-- CARGOS - UI
-------------------------------------------------

cadastrarCargoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
cadastrarCargoUI sistema = do
  cabecalho "CADASTRAR CARGO"
  cargo <- lerCargo

  case adicionarCargoValidado
         cargo
         (funcionarios sistema)
         (departamento sistema)
         (cargos sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Cargo cadastrado."
        >> return sistema { cargos = novaLista }

alterarCargoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
alterarCargoUI sistema = do
  cabecalho "ALTERAR CARGO"
  cargo <- lerCargo

  case modificarCargo cargo (cargos sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Cargo alterado."
        >> return sistema { cargos = novaLista }

buscarCargoUI :: SistemaBancoDadosRH -> IO ()
buscarCargoUI sistema = do
  cabecalho "BUSCAR CARGO"
  cid <- lerInt "ID do cargo"

  case buscarCargo cid (cargos sistema) of
    Nothing -> msgInfo "Cargo não encontrado."
    Just c  -> exibirCargo c

excluirCargoUI :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
excluirCargoUI sistema = do
  cabecalho "EXCLUIR CARGO"
  cid <- lerInt "ID do cargo"

  case excluirCargo cid (cargos sistema) of
    Left err -> msgErro err >> return sistema
    Right novaLista ->
      msgSucesso "Cargo removido."
        >> return sistema { cargos = novaLista }

listarCargosUI :: SistemaBancoDadosRH -> IO ()
listarCargosUI sistema =
  if null (cargos sistema)
    then msgInfo "Nenhum cargo cadastrado."
    else mapM_ exibirCargo (cargos sistema)

-------------------------------------------------
-- LEITURAS
-------------------------------------------------

lerFuncionario :: IO Funcionario
lerFuncionario = do
  cpf <- lerCPF
  nome <- lerLinha "Nome"
  nasc <- lerData "Data de nascimento"
  genero <- lerLinha "Gênero"
  email <- lerLinha "Email"
  telefone <- lerLinha "Telefone"
  endereco <- lerLinha "Endereço"
  cargo <- lerInt "ID do cargo"
  depto <- lerInt "ID do departamento"
  carga <- lerInt "Carga horária"
  admissao <- lerData "Data de admissão"

  return Funcionario
    { idFunc = cpf
    , nomeFunc = nome
    , dataNascimentoFunc = nasc
    , generoFunc = genero
    , emailFunc = email
    , telefoneFunc = telefone
    , enderecoFunc = endereco
    , statusFunc = Ativo
    , linkLinkedinFunc = ""
    , cargoFunc = cargo
    , deptoFunc = depto
    , historicoAlteracoesFunc = []
    , cargaHorariaFunc = carga
    , dataAdmissaoFunc = admissao
    }

lerDepartamento :: IO Departamento
lerDepartamento = do
  idD <- lerInt "ID do departamento"
  nome <- lerLinha "Nome"
  desc <- lerLinha "Descrição"
  gerente <- lerInt "ID do gerente"
  qtd <- lerInt "Qtd de funcionários"

  return Departamento
    { idDepto = idD
    , nomeDepto = nome
    , descricaoDepto = desc
    , idGerenteDepto = gerente
    , qtdFuncionarioDepto = qtd
    , registroPresencaDepto = []
    }

lerCargo :: IO Cargo
lerCargo = do
  idC <- lerInt "ID do cargo"
  nome <- lerLinha "Nome"
  funcao <- lerLinha "Função"
  carga <- lerInt "Carga horária"
  sal <- lerDouble "Salário"
  sup <- lerInt "ID do supervisor"
  depto <- lerInt "ID do departamento"

  return Cargo
    { idCargo = idC
    , nomeCargo = nome
    , funcaoCargo = funcao
    , cargaHoraria = carga
    , salario = sal
    , idSupervisor = sup
    , deptoAssociado = depto
    }

lerTipoAfastamento :: IO TipoAfastamento
lerTipoAfastamento = do
  putStrLn "1 - Médico"
  putStrLn "2 - Acidente de trabalho"
  putStrLn "3 - Licença maternidade"
  putStrLn "4 - Licença paternidade"
  putStrLn "5 - Ausência justificada"

  op <- lerLinha "Tipo"

  case op of
    "1" -> return AfastamentoMedico
    "2" -> return AcidenteDeTrabalho
    "3" -> return LicencaMaternidade
    "4" -> return LicencaPaternidade
    "5" -> return AusenciaJustificada
    _   -> msgErro "Opção inválida." >> lerTipoAfastamento

lerDocumentacaoOpcional :: IO (Maybe Documentacao)
lerDocumentacaoOpcional = do
  temDoc <- lerSN "Deseja informar documentação?"
  if not temDoc
     then return Nothing
     else do
       desc <- lerLinha "Descrição do documento"
       dataEnvio <- lerData "Data de envio do documento"
       return $ Just Documentacao
         { descricaoDocumento = desc
         , dataEnvioDocumento = dataEnvio
         }

-------------------------------------------------
-- EXIBIÇÃO
-------------------------------------------------

exibirFuncionario :: Funcionario -> IO ()
exibirFuncionario f = do
  putStrLn "--------------------------------"
  putStrLn ("CPF: " ++ idFunc f)
  putStrLn ("Nome: " ++ nomeFunc f)
  putStrLn ("Status: " ++ show (statusFunc f))
  putStrLn ("Departamento ID: " ++ show (deptoFunc f))
  putStrLn ("Cargo ID: " ++ show (cargoFunc f))

exibirDepartamento :: Departamento -> IO ()
exibirDepartamento d = do
  putStrLn "--------------------------------"
  putStrLn ("ID: " ++ show (idDepto d))
  putStrLn ("Nome: " ++ nomeDepto d)
  putStrLn ("Descrição: " ++ descricaoDepto d)
  putStrLn ("Gerente ID: " ++ show (idGerenteDepto d))
  putStrLn ("Qtd Funcionários: " ++ show (qtdFuncionarioDepto d))

exibirCargo :: Cargo -> IO ()
exibirCargo c = do
  putStrLn "--------------------------------"
  putStrLn ("ID: " ++ show (idCargo c))
  putStrLn ("Nome: " ++ nomeCargo c)
  putStrLn ("Função: " ++ funcaoCargo c)
  putStrLn ("Carga Horária: " ++ show (cargaHoraria c))
  putStrLn ("Salário: " ++ show (salario c))
  putStrLn ("Supervisor ID: " ++ show (idSupervisor c))
  putStrLn ("Departamento ID: " ++ show (deptoAssociado c))
