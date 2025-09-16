#!/bin/bash

echo "🔨 Compilando aplicação Erlang..."

# Limpar compilações anteriores
./rebar3 clean

# Baixar dependências
echo "📦 Baixando dependências..."
./rebar3 get-deps

# Compilar aplicação
echo "⚙️ Compilando código fonte..."
./rebar3 compile

if [ $? -eq 0 ]; then
    echo "✅ Compilação concluída com sucesso!"
else
    echo "❌ Erro na compilação!"
    exit 1
fi
