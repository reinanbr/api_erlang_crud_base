# API CRUD em Erlang com Teste de Carga Gatling

Esta é uma API CRUD completa em Erlang utilizando o framework Cowboy, com autenticação JWT e teste de carga usando Gatling.

## Características

- **Linguagem**: Erlang/OTP
- **Framework HTTP**: Cowboy
- **Banco de Dados**: SQLite com esqlite
- **Autenticação**: JWT (JSON Web Tokens)
- **Concorrência**: Processos Erlang isolados para cada requisição
- **Tolerância a Falhas**: Supervisor OTP para recuperação automática

## Estrutura do Projeto

```
api_erlang_crud_base/
├── src/                          # Código fonte Erlang
│   ├── api_erlang_crud.app.src   # Definição da aplicação
│   ├── api_erlang_crud_app.erl   # Módulo principal da aplicação
│   ├── api_erlang_crud_sup.erl   # Supervisor principal
│   ├── auth_utils.erl            # Utilitários de autenticação (JWT, hash)
│   ├── balance_handler.erl       # Handler para consulta de saldo
│   ├── db_server.erl             # Gen_server para banco de dados
│   ├── login_handler.erl         # Handler para login
│   ├── register_handler.erl      # Handler para registro
│   └── transaction_handler.erl   # Handler para transações (buy/sell)
├── config/
│   └── sys.config                # Configuração da aplicação
├── gatling/
│   ├── LoadTestSimulation.scala  # Script de teste de carga Gatling
│   └── gatling.conf              # Configuração do Gatling
├── scripts/
│   ├── compile.sh                # Script de compilação
│   ├── run.sh                    # Script para executar a API
│   └── test.sh                   # Script para executar os testes
├── priv/                         # Banco de dados SQLite
├── rebar.config                  # Configuração de dependências
└── README.md                     # Esta documentação
```

## Modelos de Dados

### User
- `id`: UUID único do usuário
- `username`: Nome de usuário (único)
- `password_hash`: Senha hasheada com bcrypt
- `balance`: Saldo do usuário (padrão: 1000.0)
- `created_at`: Timestamp de criação

### Transaction
- `id`: UUID único da transação
- `user_id`: UUID do usuário (chave estrangeira)
- `type`: Tipo da transação ('buy' ou 'sell')
- `amount`: Valor da transação
- `timestamp`: Timestamp da transação
- `created_at`: Timestamp de criação

## Endpoints da API

### POST /register
Cria um novo usuário.

**Payload:**
```json
{
  "username": "usuario123",
  "password": "senha123"
}
```

**Resposta (201):**
```json
{
  "success": true,
  "message": "User created successfully",
  "user_id": "550e8400-e29b-41d4-a716-446655440000"
}
```

### POST /login
Autentica um usuário e retorna um JWT.

**Payload:**
```json
{
  "username": "usuario123",
  "password": "senha123"
}
```

**Resposta (200):**
```json
{
  "success": true,
  "message": "Login successful",
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "user_id": "550e8400-e29b-41d4-a716-446655440000"
}
```

### GET /balance
Consulta o saldo do usuário autenticado.

**Headers:**
```
Authorization: Bearer <JWT_TOKEN>
```

**Resposta (200):**
```json
{
  "success": true,
  "balance": 1000.0,
  "user_id": "550e8400-e29b-41d4-a716-446655440000"
}
```

### POST /buy
Registra uma compra (debita do saldo).

**Headers:**
```
Authorization: Bearer <JWT_TOKEN>
```

**Payload:**
```json
{
  "amount": 100.50
}
```

**Resposta (200):**
```json
{
  "success": true,
  "message": "Purchase successful",
  "transaction_id": "550e8400-e29b-41d4-a716-446655440001",
  "new_balance": 899.50,
  "amount": 100.50,
  "type": "buy"
}
```

### POST /sell
Registra uma venda (credita no saldo).

**Headers:**
```
Authorization: Bearer <JWT_TOKEN>
```

**Payload:**
```json
{
  "amount": 50.25
}
```

**Resposta (200):**
```json
{
  "success": true,
  "message": "Sale successful",
  "transaction_id": "550e8400-e29b-41d4-a716-446655440002",
  "new_balance": 949.75,
  "amount": 50.25,
  "type": "sell"
}
```

## Pré-requisitos

### Para a API Erlang:
- Erlang/OTP 24+ instalado
- Rebar3 para gerenciamento de dependências

### Para os Testes Gatling:
- Java 8+ instalado
- Gatling 3.9+ instalado

## Instalação e Configuração

### 1. Instalar Erlang/OTP
```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install erlang

# CentOS/RHEL
sudo yum install erlang

# macOS (usando Homebrew)
brew install erlang
```

### 2. Instalar Rebar3
```bash
# Download e instalação do Rebar3
curl -O https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

### 3. Instalar Gatling
```bash
# Download do Gatling
wget https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle/3.9.5/gatling-charts-highcharts-bundle-3.9.5-bundle.zip
unzip gatling-charts-highcharts-bundle-3.9.5-bundle.zip
sudo mv gatling-charts-highcharts-bundle-3.9.5 /opt/gatling
sudo ln -s /opt/gatling/bin/gatling.sh /usr/local/bin/gatling
```

## Como Executar

### 1. Compilar a aplicação Erlang
```bash
# Dentro do diretório do projeto
./scripts/compile.sh
```

### 2. Executar a API
```bash
./scripts/run.sh
```

A API estará disponível em `http://localhost:8080`

### 3. Testar a API manualmente
```bash
# Registrar um usuário
curl -X POST http://localhost:8080/register \
  -H "Content-Type: application/json" \
  -d '{"username":"testuser","password":"testpass123"}'

# Fazer login
curl -X POST http://localhost:8080/login \
  -H "Content-Type: application/json" \
  -d '{"username":"testuser","password":"testpass123"}'

# Consultar saldo (substitua TOKEN pelo JWT retornado)
curl -X GET http://localhost:8080/balance \
  -H "Authorization: Bearer TOKEN"

# Fazer uma compra
curl -X POST http://localhost:8080/buy \
  -H "Authorization: Bearer TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"amount":100.50}'
```

### 4. Executar teste de carga Gatling
```bash
./scripts/test.sh
```

## Teste de Carga Gatling

O teste de carga implementado inclui:

- **10.000 registros de usuários** em 30 segundos
- **1.000 autenticações** em 10 segundos
- **500 transações** (buy/sell) em 15 segundos

### Métricas Analisadas:
- Tempo de resposta (médio, percentil 95)
- Taxa de erro (deve ser < 5%)
- Taxa de transferência (requests/segundo)
- Distribuição de tempos de resposta

### Critérios de Sucesso:
- ✅ Menos de 5% de falhas
- ✅ 95% das requisições em menos de 2 segundos
- ✅ Tempo de resposta médio < 500ms
- ✅ Registros bem-sucedidos > 95%

## Arquitetura e Concorrência

### Gen_Server para Banco de Dados
- `db_server.erl` gerencia todas as operações do banco SQLite
- Garante consistência e atomicidade das transações
- Recuperação automática em caso de falhas

### Handlers HTTP Independentes
- Cada requisição é processada em um processo Erlang separado
- Isolamento de falhas - uma requisição não afeta outras
- Concorrência natural do Erlang para milhares de conexões

### Autenticação JWT
- Tokens seguros com expiração de 1 hora
- Verificação de integridade e validade
- Stateless - não requer armazenamento no servidor

## Tolerância a Falhas

- **Supervisor Tree**: Reinicia gen_servers em caso de falha
- **Isolamento de Processos**: Falhas não se propagam
- **Transações Atômicas**: Rollback automático em caso de erro
- **Validação Rigorosa**: Validação de entrada em todas as rotas

## Monitoramento e Logs

- Logs automáticos de operações críticas
- Métricas de performance do Gatling
- Rastreamento de transações com UUID únicos

## Troubleshooting

### Problemas Comuns:

1. **Erro de compilação**:
   ```bash
   rebar3 clean
   rebar3 compile
   ```

2. **Banco de dados bloqueado**:
   ```bash
   rm priv/database.db
   # A aplicação criará um novo banco automaticamente
   ```

3. **Porta 8080 ocupada**:
   - Altere a porta em `config/sys.config`
   - Ou mate o processo: `sudo lsof -ti:8080 | xargs kill -9`

4. **Dependências não baixadas**:
   ```bash
   rebar3 upgrade
   ```

## Contribuindo

1. Fork o projeto
2. Crie uma branch para sua feature (`git checkout -b feature/AmazingFeature`)
3. Commit suas mudanças (`git commit -m 'Add some AmazingFeature'`)
4. Push para a branch (`git push origin feature/AmazingFeature`)
5. Abra um Pull Request

## Licença

Este projeto está licenciado sob a Licença Apache 2.0 - veja o arquivo LICENSE para detalhes.
