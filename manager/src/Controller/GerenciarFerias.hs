module Controller.GerenciarFerias where

import Model.TiposDados
import Data.Time
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Bool (Bool)

gerarCiclos :: Day -> [CicloFerias]
gerarCiclos admissao =
  let criar inicio =
        let fimAq = addGregorianYearsClip 1 inicio
            fimCo = addGregorianYearsClip 2 inicio
        in CicloFerias { 
            periodoAquisitivo = PeriodoAquisitivo inicio fimAq, 
            periodoConcessivo = PeriodoConcessivo fimAq fimCo, 
            saldoDias = 30, 
            faltas = 0,
            feriasDoCiclo = [], 
            statusCiclo = Esperado
          }
      prox inicio =
        let ciclo = criar inicio
            proximoInicio = addGregorianYearsClip 1 inicio
        in ciclo : prox proximoInicio
  in prox admissao

admitirFuncionario :: CPF -> Day -> GerenciadorFerias -> Either String GerenciadorFerias
admitirFuncionario cpf admissao ger
  | Map.member cpf (registros ger) = Left "Funcionário já cadastrado."
  | otherwise =
      let registro = 
            RegistroFeriasFuncionario { 
              dataAdmissao = admissao, 
              ciclos = gerarCiclos admissao
            }
      in Right ger { registros = Map.insert cpf registro (registros ger) }

atualizarStatusFerias :: Day -> Ferias -> Ferias
atualizarStatusFerias hoje f
  | hoje < inicioFerias f = f { statusFerias = Planejada }
  | hoje > fimFerias f = f { statusFerias = Concluida }
  | otherwise = f { statusFerias = EmAndamento }

atualizarStatusCiclo :: Day -> CicloFerias -> CicloFerias
atualizarStatusCiclo hoje ciclo
  | hoje > fimConcessivo (periodoConcessivo ciclo) && saldoDias ciclo > 0 = ciclo { statusCiclo = Vencido }
  | saldoDias ciclo == 0 = ciclo { statusCiclo = Concluido }
  | otherwise = ciclo { statusCiclo = Vigente }

atualizarCiclo :: Day -> CicloFerias -> CicloFerias
atualizarCiclo hoje ciclo =
  atualizarStatusCiclo hoje ciclo { 
      feriasDoCiclo = map (atualizarStatusFerias hoje) (feriasDoCiclo ciclo)
    }

calcularSaldoPorFaltas :: Int -> Int
calcularSaldoPorFaltas f
  | f <= 5   = 30
  | f <= 14  = 24
  | f <= 23  = 18
  | f <= 32  = 12
  | otherwise = 0

cicloPorData :: Day -> [CicloFerias] -> ( [CicloFerias], CicloFerias, [CicloFerias] )
cicloPorData dia ciclos =
  let (antes, atual:depois) =
        break (\c ->
          dia >= inicioAquisitivo (periodoAquisitivo c) &&
          dia <  fimAquisitivo    (periodoAquisitivo c)
        ) ciclos
  in (antes, atual, depois)

aplicarFaltasNoCiclo :: Int -> CicloFerias -> CicloFerias
aplicarFaltasNoCiclo novas ciclo =
  let totalFaltas = faltas ciclo + novas
      novoSaldo   = calcularSaldoPorFaltas totalFaltas
  in ciclo { 
      faltas = totalFaltas, saldoDias = min (saldoDias ciclo) novoSaldo
    }

registrarFaltas :: Day -> Int -> RegistroFeriasFuncionario -> Either String RegistroFeriasFuncionario
registrarFaltas dia novas reg =
  let (antes, ciclo, depois) =
        cicloPorData dia (ciclos reg)
      cicloAtualizado =
        aplicarFaltasNoCiclo novas ciclo
  in Right reg { ciclos = antes ++ cicloAtualizado : depois }

registrarFerias :: CPF -> Ferias -> Day -> GerenciadorFerias -> Either String GerenciadorFerias
registrarFerias cpf ferias hoje ger = do
  reg <- maybe
           (Left "Funcionário não encontrado.")
           Right
           (Map.lookup cpf (registros ger))
  let (antes, ciclo, depois) =
        cicloPorData (inicioFerias ferias) (ciclos reg)
  cicloFinal <- processarFerias hoje ferias ciclo
  let regAtualizado =
        reg { ciclos = antes ++ cicloFinal : depois }
  Right ger { registros = Map.insert cpf regAtualizado (registros ger) }

validarDireitoFerias :: Ferias -> CicloFerias -> Either String ()
validarDireitoFerias ferias ciclo
  | inicioFerias ferias < fimAquisitivo (periodoAquisitivo ciclo) = Left "Funcionário ainda não adquiriu direito às férias neste ciclo."
  | otherwise = Right ()

validarSaldo :: Int -> CicloFerias -> Either String ()
validarSaldo custo ciclo
  | custo > saldoDias ciclo = Left "Saldo insuficiente."
  | otherwise = Right ()

aplicarFeriasNoCiclo :: Day -> Ferias -> CicloFerias -> CicloFerias
aplicarFeriasNoCiclo hoje ferias ciclo =
  let cicloAtualizado =
        atualizarCiclo hoje ciclo
      custo = diasUtilizados ferias
  in cicloAtualizado { 
    saldoDias = saldoDias cicloAtualizado - custo, 
    feriasDoCiclo = ferias : feriasDoCiclo cicloAtualizado
  }

processarFerias :: Day -> Ferias -> CicloFerias -> Either String CicloFerias
processarFerias hoje ferias ciclo = do
  validarDireitoFerias ferias ciclo
  let cicloAtualizado =
        atualizarCiclo hoje ciclo
      feriasTotais =
        ferias : feriasDoCiclo cicloAtualizado
  validarFracionamento feriasTotais
  let custo = diasUtilizados ferias 
  validarSaldo custo cicloAtualizado
  Right (aplicarFeriasNoCiclo hoje ferias ciclo)

sobrepoe :: Ferias -> Ferias -> Bool
sobrepoe f1 f2 =
  inicioFerias f1 <= fimFerias f2 && inicioFerias f2 <= fimFerias f1

haSobreposicao :: [Ferias] -> Bool
haSobreposicao ferias =
  or [sobrepoe f1 f2 | f1 <- ferias, f2 <- ferias, f1 /= f2]

validarFracionamento :: [Ferias] -> Either String ()
validarFracionamento ferias
  | length ferias > 3 = Left "Férias não podem ser fracionadas em mais de 3 períodos."
  | haSobreposicao ferias = Left "Períodos de férias não podem se sobrepor."
  | not (any ((>= 14) . diasUtilizados) ferias) = Left "Um dos períodos de férias deve ter no mínimo 14 dias."
  | any ((< 5) . diasUtilizados) ferias = Left "Nenhum período de férias pode ter menos de 5 dias."
  | otherwise = Right ()

feriasVencidas :: Day -> CicloFerias -> Bool
feriasVencidas hoje ciclo =
  hoje > fimConcessivo (periodoConcessivo ciclo) && saldoDias ciclo > 0              

temDireitoAFerias :: Day -> CicloFerias -> Bool
temDireitoAFerias hoje ciclo =
  hoje >= fimAquisitivo (periodoAquisitivo ciclo)
  && saldoDias ciclo > 0
