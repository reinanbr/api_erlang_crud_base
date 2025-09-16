#!/bin/bash

echo "🚀 Iniciando API Erlang CRUD..."

# Verificar se a aplicação foi compilada
if [ ! -d "_build" ]; then
    echo "⚠️ Aplicação não compilada. Executando compilação..."
    ./scripts/compile.sh
fi

# Criar diretório priv se não existir
mkdir -p priv

echo "🌐 Iniciando servidor na porta 8080..."
echo "📝 Logs e saída da aplicação:"
echo "----------------------------------------"

# Executar aplicação
./rebar3 shell --config config/sys.config
