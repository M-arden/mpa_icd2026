# Arquivo: 03-lista-resolucao.R
# Autor(a): Marden José Guimarães Faria
# Data: 26/03/2026
# Objetivo: Resolução da lista de exercícios 3

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2 etc.

# Exercício 1 ------------------------------------------------

# Item a - Importa os arquivos de dados separadamente

# 1. Produtos
caminho_produtos <- here("data/raw/novos_dados/dados_produtos.csv")
dados_produtos   <- read_csv(caminho_produtos)

# 2. Vendas
caminho_vendas   <- here("data/raw/novos_dados/dados_vendas.csv")
dados_vendas     <- read_csv(caminho_vendas)

# 3. Clientes
caminho_clientes <- here("data/raw/novos_dados/dados_clientes.csv")
dados_clientes   <- read_csv(caminho_clientes)

# Inspecionando objeto

## exibe visão geral de dados
glimpse(dados_clientes)
glimpse(dados_produtos)
glimpse(dados_vendas)

# Item b - Criando o objeto relatorio_vendas
relatorio_vendas <- dados_vendas |> 
  left_join(dados_produtos, by = "codigo_produto") |> #combina vendas c/ produto
  left_join(dados_clientes, by = "id_cliente") #combina com clientes


# Item c - selecionando variáveis

relatorio_vendas <- relatorio_vendas |> 
  select(
    id_venda,
    data_venda,
    nome_produto,
    categoria,
    quantidade,
    nome_cliente,
    cidade
  )

# Visualizando o resultado final selecionado
glimpse(relatorio_vendas)


# Item d 

# Identificando registros com NA
registros_na <- relatorio_vendas %>%
  filter(if_any(everything(), is.na))

# Exibindo os registros problemáticos
print(registros_na)

# Exercício 1 - item d ---------------------------------------

# Identificando registros com NA
registros_na <- relatorio_vendas %>%
  filter(if_any(everything(), is.na))

# Exibindo os registros problemáticos
print(registros_na)

# EXPLICAÇÃO DO PORQUÊ ISSO OCORREU:
# Os NAs surgem no left_join quando uma chave presente na tabela da esquerda 
# (dados_vendas) não encontra uma correspondência exata na tabela da direita 
# (dados_produtos ou dados_clientes). 
# Isso pode acontecer por dois motivos principais:


# Item e (Opcional) ----------------------------

# 1. Refazendo com full_join
relatorio_full <- dados_vendas |> 
  full_join(dados_produtos, by = "codigo_produto")

# 2. Comparando a quantidade de linhas
nrow(relatorio_vendas) # Resultado anterior com left_join
nrow(relatorio_full)   # Novo resultado com full_join

# 3. Identificando a diferença (produtos que nunca foram vendidos)
produtos_sem_venda <- relatorio_full |> 
  filter(is.na(id_venda))


# Exercício 2 ------------------------------------------------


# Item a - Importa os arquivos de dados separadamente

# 1. Governança
caminho_governanca <- here("data/raw/novos_dados/dados_governanca.csv")
dados_governanca  <- read_csv(caminho_governanca)

# 2. Risco
caminho_risco   <- here("data/raw/novos_dados/dados_risco.csv")
dados_risco    <- read_csv(caminho_risco)

# 3. Contábeis
caminho_contabeis <- here("data/raw/novos_dados/dados_contabeis.csv")
dados_contabeis   <- read_csv(caminho_contabeis)

# Inspecionando objeto

## exibe visão geral de dados
glimpse(dados_governanca)
glimpse(dados_risco)
glimpse(dados_contabeis)


# Item b

# Criando a análise integrada unindo as três bases
analise_integrada <- dados_governanca |> 
  # Passo 1: Combinar governança com risco
  # (Geralmente a chave comum é o ID da empresa ou código da ação)
  left_join(dados_risco, by = "codigo_negociacao") |> 
  # Passo 2: Combinar o resultado com dados contábeis
  left_join(dados_contabeis, by = "codigo_negociacao")

# Verificando a estrutura do novo objeto
glimpse(analise_integrada)

names(dados_governanca)
names(dados_risco)
names(dados_contabeis)

# Item c ---------------------------------------

# Exercício 1 - item c ---------------------------------------

# Selecionando as variáveis específicas do setor financeiro/governança
analise_integrada <- analise_integrada %>%
  select(
    empresa,
    codigo_negociacao,
    ano,
    indice_governanca,
    tipo_controlador,
    comite_auditoria,
    retorno_anual,
    volatilidade,
    beta,
    roa,
    alavancagem,
    tamanho_ativo
  )

# Exibindo a estrutura final para conferência
glimpse(analise_integrada)

# Item d ---------------------------------------

# EXPLICAÇÃO SOBRE VALORES NA:
# 1. Os valores NA (Not Available) surgem quando uma empresa presente na 
#    base de Governança (nossa tabela principal) não possui um registro 
#    correspondente nas tabelas de Risco ou Contábil.

# POR QUE O LEFT_JOIN() É ADEQUADO:
# 1. O left_join() prioriza a base da esquerda (a primeira declarada). 
# 2. Com o left_join, mantemos a lista completa e podemos decidir 
#    posteriormente como tratar os dados faltantes (ex: excluir ou imputar).


# Exercício 3 ------------------------------------------------

# Item b

# Importação do arquivo de ações
caminho_acoes <- here("data/raw/novos_dados/dados_acoes.csv")
dados_acoes <- read_csv(caminho_acoes)

# Importação do arquivo de eventos corporativos
caminho_eventos <- here("data/raw/novos_dados/dados_eventos_corporativos.csv")
dados_eventos_corporativos <- read_csv(caminho_eventos)

# Análise da estrutura dos objetos importados
glimpse(dados_acoes)
glimpse(dados_eventos_corporativos)


# Item c

# Criando a combinação das tabelas com inner_join
dados_estudo_eventos <- dados_acoes %>%
  inner_join(
    dados_eventos_corporativos, 
    by = c("ticker" = "ticker", "data" = "data_anuncio")
  )

# Visualizando o resultado
glimpse(dados_estudo_eventos)


# Selecionando as variáveis específicas para o estudo de eventos
dados_estudo_eventos <- dados_estudo_eventos %>%
  select(
    ticker,
    data,
    tipo_evento,
    valor,
    retorno_diario,
    volume
  )

# Exibindo o objeto final
print(dados_estudo_eventos)
# Ou use view(dados_estudo_eventos) para abrir em uma nova aba

# Item d 
  
# EXPLICAÇÃO SOBRE O NÚMERO DE LINHAS:
# 1. O objeto final possui menos linhas que 'dados_acoes' porque a tabela 
# original de ações contém o histórico diário de preços de vários anos. 
# Todos os dias de negociação "comuns" (sem eventos) foram descartados.
  
# POR QUE O INNER_JOIN() NÃO MANTÉM NÃO CORRESPONDIDOS:
# 1. A função inner_join() funciona por intersecção (lógica de conjuntos). 
#    Ela exige que a chave (neste caso, ticker + data) exista simultaneamente 
#    nas duas tabelas.
# 2. Se um ticker/data existe em 'dados_acoes' mas não há evento registrado 
#    para esse dia em 'dados_eventos_corporativos', o inner_join entende que 
#    essa linha não interessa para a análise e a remove.

# Fim ---------------------------------------------------------------------
