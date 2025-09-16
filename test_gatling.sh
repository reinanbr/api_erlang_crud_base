#!/bin/bash

# Verificar se o Gatling está instalado
if ! command -v gatling &> /dev/null; then
    echo "❌ Gatling não encontrado. Instalando..."
    
    # Download do Gatling
    GATLING_VERSION="3.9.5"
    GATLING_DIR="gatling-charts-highcharts-bundle-${GATLING_VERSION}"
    GATLING_ZIP="${GATLING_DIR}-bundle.zip"
    
    if [ ! -f "$GATLING_ZIP" ]; then
        echo "📥 Baixando Gatling ${GATLING_VERSION}..."
        wget "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle/${GATLING_VERSION}/${GATLING_ZIP}"
    fi
    
    if [ ! -d "$GATLING_DIR" ]; then
        echo "📦 Extraindo Gatling..."
        unzip -q "$GATLING_ZIP"
    fi
    
    # Usar Gatling local
    GATLING_HOME="$(pwd)/${GATLING_DIR}"
    GATLING_CMD="${GATLING_HOME}/bin/gatling.sh"
else
    GATLING_CMD="gatling"
fi

echo "🏃 Executando teste de carga com Gatling..."
echo "📍 Arquivo de simulação: gatling/LoadTestSimulation.scala"
echo "🎯 Target: http://localhost:8081"
echo "📈 Usuários: 10.000 em 30 segundos"
echo ""

# Verificar se a API está rodando
if ! curl -s http://localhost:8081/register > /dev/null; then
    echo "⚠️ API não está rodando em localhost:8081"
    echo "💡 Execute primeiro: ./scripts/run.sh"
    exit 1
fi

# Configurar Gatling para usar nossos arquivos
export GATLING_CONF_DIR="$(pwd)/gatling"
export GATLING_SIMULATIONS_DIR="$(pwd)/gatling"

# Executar teste
if [ -x "$GATLING_CMD" ]; then
    $GATLING_CMD -sf gatling -s LoadTestSimulation
else
    echo "❌ Não foi possível executar o Gatling"
    exit 1
fi

echo ""
echo "✅ Teste de carga concluído!"
echo "� Verifique os relatórios gerados na pasta results/"
