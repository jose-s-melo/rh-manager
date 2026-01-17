module Controller.Afastamento where

import Model.TiposDados
import Data.Time(Day)
import Controller.GerenciaFuncionarios (buscarFuncionario, modificarFuncionario)

-- Valida se o período de afastamento é válido (data de início anterior ou igual à data de fim).
validarPeriodoAfastamento :: Afastamento -> Bool
validarPeriodoAfastamento a = dataInicio a <= dataFim a

-- Verifica se dois afastamentos conflitam (mesmo funcionário e períodos que se sobrepõem).
conflita :: Afastamento -> Afastamento -> Bool
conflita a1 a2 =
    cpfFuncionario a1 == cpfFuncionario a2 &&
    not (dataFim a1 < dataInicio a2 || dataFim a2 < dataInicio a1)


registrarAfastamento :: Afastamento -> [Funcionario] -> [Afastamento] -> Either String ([Funcionario],[Afastamento])
registrarAfastamento afastamento funcionarios afastamentos =
    case buscarFuncionario (cpfFuncionario afastamento) funcionarios of
        Nothing -> Left "Erro: Funcionário não encontrado!"
        Just func -> 
            if statusFunc func /= Ativo
                then Left "Erro: Afastamento só pode ser registrado para funcionário ativo."
                else 
                    if not (validarPeriodoAfastamento afastamento)
                    then Left "Erro: Período de afastamento inválido."
                    else
                        if any (conflita afastamento) afastamentos
                        then Left "Erro: Conflito com outro afastamento existente."
                        else
                            let funcAtualizado = func {statusFunc = Afastado}
                            in case modificarFuncionario funcAtualizado funcionarios of
                                Left msg -> Left msg
                                Right novaListaFuncionarios -> 
                                    Right (novaListaFuncionarios, afastamento : afastamentos)

encerrarAfastamento :: Id -> [Funcionario] -> [Afastamento] -> Either String ([Funcionario],[Afastamento])
encerrarAfastamento idAf funcionarios afastamentos =
    case buscarAfastamento idAf afastamentos of
        Nothing -> Left "Erro: Afastamento não encontrado."
        Just afastamento ->
            case buscarFuncionario (cpfFuncionario afastamento) funcionarios of
                Nothing -> Left "Erro: Funcionário não encontrado."
                Just func ->
                    if statusFunc func /= Afastado
                    then Left "Erro: Funcionário não está afastado."
                    else
                        let funcAtualizado = func {statusFunc = Ativo}
                            novaListaAfastamentos = filter (\a -> idAfastamento a /= idAf) afastamentos
                        in case modificarFuncionario funcAtualizado funcionarios of
                            Left msg -> Left msg
                            Right novaListaFuncionarios ->
                                Right (novaListaFuncionarios, novaListaAfastamentos)

buscarAfastamento :: Id -> [Afastamento] -> Maybe Afastamento
buscarAfastamento idAf =
  find (\a -> idAfastamento a == idAf)                            