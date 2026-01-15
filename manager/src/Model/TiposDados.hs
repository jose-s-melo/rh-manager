module Model.TiposDados where

import Data.Time(Day)

type CPF = String
type Id = Int
type Registro = [String]

data Cargo = Cargo{
    idCargo :: Id,
    nomeCargo :: String,
    funcaoCargo :: String,
    cargaHoraria :: Int,
    salario :: Double,
    idSupervisor :: Id,
    deptoAssociado :: Id
} deriving (Show,Read, Eq)

data Departamento = Departamento{
    idDepto :: Id,
    nomeDepto :: String,
    descricaoDepto :: String,
    idGerenteDepto :: Id, -- Chave estrangeira. Referência ao Funcionario.
    qtdFuncionarioDepto :: Int,
    registroPresencaDepto :: Registro -- Esse aqui não sei bem qual tipo...
} deriving (Show,Read,Eq)

data Status = Ativo |  Afastado | Desligado deriving(Show,Read,Eq)

data Funcionario = Funcionario{
    idFunc :: CPF,
    nomeFunc :: String,
    dataNascimentoFunc :: Day,
    generoFunc :: String,
    emailFunc :: String,
    telefoneFunc :: String,
    enderecoFunc :: String,
    statusFunc :: Status,
    linkLinkedinFunc :: String,
    cargoFunc :: Id, -- Chave estrangeira. Referência ao Cargo.
    deptoFunc :: Id, -- Chave estrangeira. Referência ao Departamento.
    historicoAlteracoesFunc :: [String],
    cargaHorariaFunc :: Int, -- Chave estrangeira ???
    dataAdmissaoFunc :: Day

} deriving (Show, Read, Eq)

data SistemaBancoDadosRH = SistemaBancoDadosRH {
    funcionarios :: [Funcionario],
    cargos :: [Cargo],
    departamento :: [Departamento]
} deriving (Show, Read)