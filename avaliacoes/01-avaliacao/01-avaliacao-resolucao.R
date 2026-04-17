# Arquivo: 01-avaliacao-resolucao.R
# Autor(a): [Marden José Guimarães Faria]
# Data: 16/04/2026
# Objetivo: 
# Resolução da Avaliação 1 - Introdução à Ciência de Dados


# Configurações globais  ----------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários

library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2 etc.

# Resolução da Questão 1


# 1.a) --------------------------------------------------------

# define os caminhos relativos dos arquivos
caminho_agencias <- here("data/raw/agencias.csv")
caminho_credito_trimestral <- here("data/raw/credito_trimestral.csv")
caminho_inadimplencia <- here("data/raw/inadimplencia.csv")

# importa os arquivos
dados_agencias <- read_csv(caminho_agencias)
dados_credito_trimestral <- read_csv(caminho_credito_trimestral)
dados_inadimplencia <- read_csv(caminho_inadimplencia)

# analisa os objetos importados
glimpse(dados_agencias)
glimpse(dados_credito_trimestral)
glimpse(dados_inadimplencia)

# 1.b) ---------------------------------------------------------

# Reorganizando os dados

# Assumindo que todas as colunas exceto 'codigo_agencia' sejam os trimestres
credito_long <- credito_trimestral |> 
  pivot_longer(
    cols = "-codigo_agencia", 
    names_to = "trimestre", 
    values_to = "volume_credito"
  )

# Imprimindo o resultado no console
print(credito_long)


# 2. Imprimindo o resultado no console

# Reorganizando os dados para o formato long
credito_long <- credito_trimestral |> 
  pivot_longer(
    cols = -codigo_agencia,        
    names_to = "trimestre",        
    values_to = "volume_credito"   
  )


# 1.c) ---------------------------------------------------------

# Reunindo as informações em um único objeto

dados_completos <- credito_long |>
  left_join(agencias, by = "codigo_agencia")
  left_join(inadimplencia, by = c("credito_long", "codigo_agencia"))

# 1.d) ---------------------------------------------------------

# Criando novas variáveis
  
# Variável 1
  
dados_completos <- credito_por_cooperado |> 
    mutate(credito_por_cooperado = volume_credito * 1000 / num_cooperados)
  
# Variável 2

  
dados_analise <- dados_completos |> 
  situacao = case_when (
  taxa_inadimplencia < 3.0 ~ "Baixo"
  taxa_inadimplencia >= 3.0 & 4.5 ~ "Moderado"
  taxa_inadimplencia >= 4.5 -> "Alto"
)

# 1.e) ---------------------------------------------------------

analise_inadimplencia <- dados_analise |> 
  mutate(
    credito_por_cooperado = volume_credito,

    )





# Resolução da Questão 2


# 2.a) ---------------------------------------------------------



# 2.b) ---------------------------------------------------------



# 2.c) ---------------------------------------------------------