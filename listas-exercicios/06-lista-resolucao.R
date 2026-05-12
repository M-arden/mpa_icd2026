# Arquivo: 06-lista-resolucao.R
# Autor(a): Marden J. G. Faria
# Data: 10/05/2026
# Objetivo: Resolução da lista de exercícios 6

# Exercício 1 ------------------------------------------------

library(tidyverse)

set.seed(20260507)

# Definição dos parâmetros
retornos_possiveis <- c(0.06, 0.02, -0.01, -0.04)
probabilidades <- c(0.15, 0.45, 0.25, 0.15)

# Simulação de 10.000 meses
retornos_simulados <- sample(retornos_possiveis, size = 10000, 
                             replace = TRUE, prob = probabilidades)

# Estimativas
media <- mean(retornos_simulados)
variancia <- var(retornos_simulados)
desvio_padrao <- sd(retornos_simulados)
prob_negativo <- mean(retornos_simulados < 0)

# Exibição dos resultados
cat("Valor Esperado:", media, "\nDesvio-Padrão:",
    desvio_padrao, "\nProb. Retorno Negativo:", prob_negativo)


# Exercício 2 ------------------------------------------------

set.seed(20260508)

lucros_possiveis <- c(900, 150, -3500)
probs_credito <- c(0.88, 0.08, 0.04)

# Parte 1: 20.000 operações individuais
ops_individuais <- sample(lucros_possiveis, size = 20000, 
                          replace = TRUE, prob = probs_credito)

# Parte 2: Simular 5.000 carteiras (cada uma com 80 operações)
# A função replicate repete a soma de 80 sorteios 5 mil vezes
lucro_carteiras <- replicate(5000, sum(sample(lucros_possiveis,
                                              size = 80, replace = TRUE,
                                              prob = probs_credito)))

# Comparação
lucro_medio_individual <- mean(ops_individuais)
lucro_medio_carteira <- mean(lucro_carteiras)

cat("Lucro Médio por Operação:", lucro_medio_individual, 
    "\nLucro Médio por Carteira:", lucro_medio_carteira,
    "\n80x o Lucro Médio Individual:", 80 * lucro_medio_individual)

# Exercício 3 ------------------------------------------------

set.seed(20260509)

# 1. Sortear os estados da economia para 20.000 meses
base_economia <- tibble(
  estado = sample(c("expansao", "recessao"), size = 20000, 
                  replace = TRUE, prob = c(0.7, 0.3))
)

# 2. Sortear o retorno condicionado ao estado sorteado
base_economia <- base_economia %>%
  mutate(retorno = map_dbl(estado, ~ {
    if (.x == "expansao") {
      sample(c(0.09, 0.04, -0.02), size = 1, prob = c(0.25, 0.50, 0.25))
    } else {
      sample(c(0.03, -0.04, -0.11), size = 1, prob = c(0.15, 0.45, 0.40))
    }
  }))

# Resultados
media_global <- mean(base_economia$retorno)
medias_por_estado <- base_economia %>% group_by(estado) %>% summarise(
  media = mean(retorno))
prob_neg_total <- mean(base_economia$retorno < 0)


# Exercício 4 ------------------------------------------------

set.seed(20260510)

# Simulação do cenário de mercado
base_ativos <- tibble(
  cenario = sample(c("boom", "estabilidade", "crise"),
                   size = 15000, replace = TRUE, prob = c(0.2, 0.5, 0.3))
) %>%
  mutate(
    RA = case_when(cenario == "boom" ~ 0.12, 
                   cenario == "estabilidade" ~ 0.03, TRUE ~ -0.09),
    RB = case_when(cenario == "boom" ~ 0.07, 
                   cenario == "estabilidade" ~ 0.02, TRUE ~ -0.04),
    XP = 0.6 * RA + 0.4 * RB # Retorno da Carteira
  )

# Estatísticas de Diversificação
correlacao <- cor(base_ativos$RA, base_ativos$RB)
sd_A <- sd(base_ativos$RA)
sd_B <- sd(base_ativos$RB)
sd_Carteira <- sd(base_ativos$XP)

# Exercício 5 ------------------------------------------------

set.seed(20260511)

simular_mes <- function() {
  # 1. Sorteia o cenário do mês
  cen <- sample(c("aquecido", "normal", "fraco"), size = 1, 
                prob = c(0.25, 0.50, 0.25))
  
  # 2. Define regras baseadas no cenário
  n_contratos <- switch(cen, "aquecido" = 120, "normal" = 90, "fraco" = 60)
  p_inad <- switch(cen, "aquecido" = 0.03, "normal" = 0.06, "fraco" = 0.10)
  
  # 3. Simula contratos (adimplentes vs inadimplentes)
  inadimplentes <- rbinom(1, n_contratos, p_inad)
  lucro = ((n_contratos - inadimplentes) * 300) - (inadimplentes * 2500)
  
  return(tibble(cenario = cen, n_inad = inadimplentes, lucro = lucro))
}

# 4. Executa 10.000 simulações e junta os dados
base_final <- replicate(10000, simular_mes(), simplify = FALSE) %>% bind_rows()

# Estatísticas
correlacao_calote_lucro <- cor(base_final$n_inad, base_final$lucro)
prob_prejuizo <- mean(base_final$lucro < 0)


# Fim------------------------------------------------