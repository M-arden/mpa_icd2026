# Arquivo: 04-lista-resolucao.R
# Autor(a): Marden J. G. Faria
# Data: 14/04/2026
# Objetivo: Resolução da lista de exercícios 4

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2 etc.


# Exercício 1 ------------------------------------------------

# Função montante mensal
calcular_montante_mensal <- function(capital, taxa_anual, meses) {
  taxa_mensal <- taxa_anual / 12
  montante <- capital * (1 + taxa_mensal)^meses
  return(montante)
}

# Teste
calcular_montante_mensal(capital = 5000, taxa_anual = 0.10, meses = 36)
# Resultado esperado: ~6741.74

# Exercício 2 ------------------------------------------------

# Avaliação de investimento
avaliar_investimento <- function(retorno) {
  if (retorno > 0.15) {
    return("Excelente")
  } else if (retorno > 0.05) {
    return("Bom")
  } else if (retorno > 0) {
    return("Fraco")
  } else {
    return("Negativo")
  }
}

# Testes
map_chr(c(0.20, 0.08, 0.02, -0.05), avaliar_investimento)


# Exercício 3 ------------------------------------------------

# Analise da carteira
analisar_carteira <- function(dados) {
  dados %>%
    mutate(
      retorno = (preco_atual - preco_compra) / preco_compra,
      valor_investido = preco_compra * quantidade,
      valor_atual = preco_atual * quantidade,
      resultado = valor_atual - valor_investido,
      situacao = ifelse(resultado > 0, "Ganho", "Perda")
    )
}

# Tibble de teste
carteira <- tibble(
  ativo        = c("PETR4", "VALE3", "ITUB4", "WEGE3"),
  preco_compra = c(28.50, 68.20, 32.00, 45.00),
  preco_atual  = c(31.00, 65.40, 33.60, 48.50),
  quantidade   = c(200, 100, 300, 150)
)

analisar_carteira(carteira)

# Exercício 4 ------------------------------------------------

# Comparação de cenários
calcular_valor_futuro <- function(capital, taxa_anual, anos) {
  capital * (1 + taxa_anual)^anos
}

taxas_anuais <- c(0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16)

vf_20_anos <- map_dbl(taxas_anuais, ~calcular_valor_futuro(10000, .x, 20))

comparacao_cenarios <- tibble(
  taxa = taxas_anuais,
  taxa_percentual = paste0(taxas_anuais * 100, "%"),
  valor_futuro = vf_20_anos,
  ganho_liquido = valor_futuro - 10000
)

print(comparacao_cenarios)

# Exercício 5 ------------------------------------------------

# Análise de VPL de projeto
calcular_vpl <- function(investimento_inicial, fluxos, taxa, valor_residual = 0) {
  tempos <- 1:length(fluxos)
  vpl_fluxos <- sum(fluxos / (1 + taxa)^tempos)
  vpl_residual <- valor_residual / (1 + taxa)^max(tempos)
  vpl_total <- vpl_fluxos + vpl_residual - investimento_inicial
  return(vpl_total)
}

fluxos_projeto <- c(80000, 95000, 110000, 100000)
taxas_desconto <- c(0.08, 0.10, 0.12, 0.14, 0.16, 0.18)

analise_projeto <- tibble(
  taxa_pct = taxas_desconto,
  vpl = map_dbl(taxas_desconto, ~calcular_vpl(300000, fluxos_projeto, .x, 30000))
) %>%
  mutate(decisao = ifelse(vpl > 0, "Viável", "Inviável"))

print(analise_projeto)


# Exercício 6 ------------------------------------------------
# (resolver em arquivo .qmd separado)


# Exercício 7 (Desafio) --------------------------------------

