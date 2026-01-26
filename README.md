# Sistema RH em Haskell

Sistema de **RH MANAGER** desenvolvido em **Haskell** como parte das atividades de
**Paradigmas de Linguagens de Programação (PLP)**, com arquitetura modular, regras
de negócio e interação via terminal.

O projeto unifica **gestão de funcionários**, **departamentos**, **cargos**, 
**presença**, **afastamentos**, **licenças**, **jornadas** e **férias** em uma única
interface.

---

## Arquitetura do Projeto

O projeto segue uma **arquitetura em camadas**, evitando acoplamento excessivo e
facilitando manutenção e evolução.

### Camadas principais:

- **Model**
  - Tipos de dados do domínio (Funcionario, Cargo, Departamento, Ferias, Presenca, etc)
- **Controller**
  - Regras de negócio puras
  - Validações, cálculos e transformações de estado
- **Util**
  - Interfaces de usuário (UI)
  - Leitura de dados, exibição e menus
- **System**
  - Orquestração geral do sistema (`System.SistemaRH`)
  - Integração entre todos os subsistemas

O `Main` apenas inicializa o sistema, mantendo toda a lógica fora dele.

---

## Funcionalidades

### Funcionários
- Cadastro, alteração, busca e exclusão
- Desligamento de funcionário
- Controle de status (Ativo, Afastado, Desligado)
- Listagem de funcionários ativos, afastados e desligados

### Departamentos
- Cadastro, alteração, busca e exclusão
- Associação com funcionários

### Cargos
- Cadastro, alteração, busca e exclusão
- Associação obrigatória a um departamento
- Definição de carga horária e salário

### Presença
- Registro diário de presença
- Suporte a modalidade presencial e remoto
- Registro de faltas justificadas e injustificadas
- Consulta por dia
- Histórico completo por funcionário
- Cálculo de horas trabalhadas em um período
- Cálculo de faltas injustificadas

### Afastamentos
- Registro de afastamentos por tipo de necessidade
- Exigência automática de documentação quando aplicável
- Encerramento de afastamentos
- Listagem de funcionários afastados

### Licenças e Jornadas
- Registro e remoção de licenças
- Criação de escalas semanais
- Controle de jornadas de trabalho
- Consulta de próximas folgas

### Férias
- Controle completo de ciclos aquisitivos e concessivos
- Verificação automática de direito a férias
- Registro de férias simples ou fracionadas
- Atualização automática de saldo de dias
- Exibição detalhada dos ciclos
- Validação por data de referência

---

## Execução

Clone o repositório com:

`git clone https://github.com/jose-s-melo/rh-manager.git`

Entre na raiz do projeto e execute:

`cabal run`
