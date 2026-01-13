model Controller.GerenciaFuncionarios where

import Data.List(find)
import Model.TiposDados
-- Recebe um CPF. Verifica se é algum ID de algum funcionário na lista.
existeFuncionario :: String -> [Funcionario] -> Bool
existeFuncionario idPossivel listaFuncionarios = undefined

-- Recebe um novo funcionário e acrescenta-o na lista dos funcionários. Retorna a lista após a adição.
adicionarFuncionario :: Funcionario -> [Funcionario] -> [Funcionario]
adicionarFuncionario novoFuncionario listaFuncionarios = novoFuncionario : listaFuncionarios

--  Recebe um funcionário com ID já existente na lista e com novos dados. Retira esse "antigo" e adiciona o "novo" funcionário.
modificarFuncionario :: Funcionario -> [Funcionario] -> [Funcionario]
modificarFuncionario novoFuncionario listaFuncionarios = undefined

-- Busca um funcionário pelo seu ID. Retorna-o se encontrar ou retorna nothing, caso não exista na lista.
buscarFuncionario :: String -> [Funcionario] -> Maybe Funcionario
buscarFuncionario idBuscado listaFuncionarios = undefined

-- Busca um funcionário pelo seu ID. Em ambos casos (se estiver ou não na lista), retorna a lista após essa operação. 
excluirFuncionario :: String -> [Funcionario] -> [Funcionario]
excluirFuncionario idParaRemocao listaFuncionarios = undefined

exibirFuncionarioAtivo :: [Funcionario] -> [Funcionario]
exibirFuncionarioAtivo lista = undefined

