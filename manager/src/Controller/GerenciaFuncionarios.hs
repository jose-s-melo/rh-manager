module Controller.GerenciaFuncionarios where

import Data.List(find)
import Model.TiposDados
import Data.Char(isDigit)

cpfValido :: String -> Bool
cpfValido cpf = (length cpf == 11) && (all isDigit cpf)
-- Recebe um CPF. Verifica se é algum ID da lista.


existeFuncionario :: String -> [Funcionario] -> Bool
existeFuncionario _ [] = False
existeFuncionario idPossivel (l:ls) 
    | (idFunc l == idPossivel) = True
    | otherwise = existeFuncionario idPossivel ls

-- Recebe um novo funcionário e acrescenta-o na lista dos funcionários. Retorna a lista após a adição.
adicionarFuncionario :: Funcionario -> [Funcionario] -> Either String [Funcionario]
adicionarFuncionario novoFuncionario listaFuncionarios 
    | not (cpfValido (idFunc novoFuncionario)) = Left "Erro: Formato CPF incorreto! Deve ser inserido 11 digitos! "
    | existeFuncionario (idFunc novoFuncionario) listaFuncionarios == True = Left "Erro: Já existe  funcionário com o CPF referido!"
    | otherwise = Right (novoFuncionario : listaFuncionarios)

--  Recebe um funcionário com ID já existente na lista e com novos dados. Retira esse "antigo" e adiciona o "novo" funcionário.
modificarFuncionario :: Funcionario -> [Funcionario] -> Either String [Funcionario]
modificarFuncionario _ [] = Left "Erro: Funcionário não encontrado para realizar alteração de dados!"
modificarFuncionario novoFuncionario (l:ls)
    | (idFunc novoFuncionario == idFunc l) = Right  (novoFuncionario : ls)
    | otherwise = case modificarFuncionario novoFuncionario ls of
                  Left msg -> Left msg
                  Right novaLista -> Right (l : novaLista)

-- Busca um funcionário pelo seu ID. Retorna-o se encontrar ou retorna nothing, caso não exista na lista.
buscarFuncionario :: String -> [Funcionario] -> Maybe Funcionario
buscarFuncionario _ [] = Nothing
buscarFuncionario idBuscado (l:ls) 
    | (idFunc l == idBuscado ) = Just l
    | otherwise = buscarFuncionario idBuscado ls



-- Excluir um funcionário pelo seu ID. Em ambos casos (se estiver ou não na lista), retorna a lista após essa operação. 
excluirFuncionario :: String -> [Funcionario] -> Either String [Funcionario]
excluirFuncionario _ [] = Left "Funcionário inexistente!"
excluirFuncionario idParaRemocao (l:ls) 
    | (idFunc l == idParaRemocao) = Right ls
    | otherwise =  case excluirFuncionario idParaRemocao ls of
                    Left msg -> Left msg -- Erro subindo na recursão.
                    Right novaLista -> Right (l : novaLista) -- Devolução de lista (tinha o ID para ser removido.)


exibirFuncionarioAtivo :: [Funcionario] -> [Funcionario]
exibirFuncionarioAtivo [] = []
exibirFuncionarioAtivo (l:ls)
    | statusFunc l == Ativo = l : exibirFuncionarioAtivo ls
    | otherwise = exibirFuncionarioAtivo ls

exibirFuncionarioAfastado :: [Funcionario] -> [Funcionario]
exibirFuncionarioAfastado [] = []
exibirFuncionarioAfastado (l:ls) 
    | statusFunc l == Afastado = l : exibirFuncionarioAfastado ls
    | otherwise = exibirFuncionarioAfastado ls

exibirFuncionarioDesligado :: [Funcionario] -> [Funcionario]
exibirFuncionarioDesligado [] = []
exibirFuncionarioDesligado (l:ls)
    | statusFunc l == Desligado = l : exibirFuncionarioDesligado ls
    | otherwise = exibirFuncionarioDesligado ls

