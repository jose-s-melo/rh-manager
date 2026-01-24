module Util.BdService where

import Controller.GerenciaFuncionarios
import Controller.GerenciaDepartamentos
import Controller.GerenciaCargos
import Model.TiposDados
import Util.Utilitarios
import Data.Time

-------------------------------------------------
-- SISTEMA PRINCIPAL
-------------------------------------------------

iniciarSistemaBD :: IO ()
iniciarSistemaBD =
  loop (SistemaBancoDadosRH [] [] [])

loop :: SistemaBancoDadosRH -> IO ()
loop sistema = do
  cabecalho "MENU - SISTEMA DE BANCO DE DADOS RH"

  putStrLn "1 - Gerenciar funcionários"
  putStrLn "2 - Gerenciar departamentos"
  putStrLn "3 - Gerenciar cargos"
  putStrLn "0 - Sair"

  opcao <- lerLinha "Selecione uma opção"

  case opcao of
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
  cabecalho "FUNCIONÁRIOS"

  putStrLn "1 - Cadastrar"
  putStrLn "2 - Alterar"
  putStrLn "3 - Buscar"
  putStrLn "4 - Excluir"
  putStrLn "5 - Listar"
  putStrLn "0 - Voltar"

  op <- lerLinha "Escolha"

  case op of
    "1" -> cadastrarFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "2" -> alterarFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "3" -> buscarFuncionarioUI sistema >> pause >> menuFuncionariosLoop sistema
    "4" -> excluirFuncionarioUI sistema >>= pausaEVolta menuFuncionariosLoop
    "5" -> listarFuncionariosUI sistema >> pause >> menuFuncionariosLoop sistema
    "0" -> return sistema
    _   -> msgErro "Opção inválida." >> pause >> menuFuncionariosLoop sistema

-------------------------------------------------
-- MENU DEPARTAMENTOS
-------------------------------------------------

menuDepartamentosLoop :: SistemaBancoDadosRH -> IO SistemaBancoDadosRH
menuDepartamentosLoop sistema = do
  cabecalho "DEPARTAMENTOS"

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
  cabecalho "CARGOS"

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
