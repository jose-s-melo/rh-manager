module Controller.GerenciaDepartamentos where

import Model.TiposDados
import Controller.ConsultasBasicas

adicionarDepartamento :: Departamento -> [Departamento] -> Either String [Departamento]
adicionarDepartamento novoDepto listaDeptos
    | existeDepartamento (idDepto novoDepto) listaDeptos =
        Left "Erro: Já existe um departamento com o ID informado!"
    | qtdFuncionarioDepto novoDepto < 0 =
        Left "Erro: Quantidade de funcionários inválida!"
    | otherwise =
        Right (novoDepto : listaDeptos)

adicionarDepartamentoValidado
    :: Departamento
    -> [Funcionario]
    -> [Departamento]
    -> Either String [Departamento]
adicionarDepartamentoValidado novoDepto funcionarios departamentos
    | gerenteInvalido =
        Left "Erro: Gerente informado não existe no sistema!"
    | existeDepartamento (idDepto novoDepto) departamentos =
        Left "Erro: Já existe departamento com esse ID!"
    | otherwise =
        Right (novoDepto : departamentos)
  where
    gerenteInvalido =
      case idGerenteDepto novoDepto of
        Nothing   -> False
        Just idG  -> not (existeFuncionario (show idG) funcionarios)


modificarDepartamento :: Departamento -> [Departamento] -> Either String [Departamento]
modificarDepartamento _ [] =
    Left "Erro: Departamento não encontrado para alteração!"
modificarDepartamento novoDepto (d:ds)
    | idDepto d == idDepto novoDepto =
        Right (novoDepto : ds)
    | otherwise =
        case modificarDepartamento novoDepto ds of
            Left msg -> Left msg
            Right novaLista -> Right (d : novaLista)

buscarDepartamento :: Id -> [Departamento] -> Maybe Departamento
buscarDepartamento _ [] = Nothing
buscarDepartamento idBuscado (d:ds)
    | idDepto d == idBuscado = Just d
    | otherwise = buscarDepartamento idBuscado ds

excluirDepartamento :: Id -> [Departamento] -> Either String [Departamento]
excluirDepartamento _ [] =
    Left "Erro: Departamento inexistente!"
excluirDepartamento idRemocao (d:ds)
    | idDepto d == idRemocao =
        Right ds
    | otherwise =
        case excluirDepartamento idRemocao ds of
            Left msg -> Left msg
            Right novaLista -> Right (d : novaLista)
