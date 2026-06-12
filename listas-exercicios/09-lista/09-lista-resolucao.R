# Arquivo: 09-lista-resolucao.R
# Autor(a): Marden J. G. Faria
# Data: 28/05/2026
# Objetivo: Resolução da lista de exercícios 9

# Exercício 1 ------------------------------------------------

# Carregar pacotes

library(dplyr)
library(tidyr)
library(tidyquant)

# Importa as séries de preços das ações
serie_precos <- c("PETR4.SA") |>  
  tq_get(from = "2024-01-01") |> 
  select(symbol, date, close) |> 
  pivot_wider(names_from = symbol, values_from = close) |> 
  rename(dia = date,
         petr4 = PETR4.SA
         )

# Calcula retornos logarítmicos
retornos <- serie_precos |> 
  mutate(
    ret_petr4 = log(petr4 / lag(petr4))
  ) |> 
  drop_na()

# Define os pesos
pesos <- c(0.4, 0.3, 0.3)  # exemplo: 40% PETR4, 30% MGLU3, 30% ITUB4

# Calcula o retorno da carteira
retornos <- retornos %>%
  mutate(
    ret_carteira = pesos[1] * ret_petr4 +
      pesos[2] * ret_mglu3 +
      pesos[3] * ret_itub4
  )

# dimensão do retorno da carteira
dim(retornos$ret_carteira)

# exibie as primeiras linhas
head(retornos$ret_carteira)