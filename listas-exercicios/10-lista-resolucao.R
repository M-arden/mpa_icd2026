# Arquivo: 10-lista-resolucao.R
# Autor(a): Marden J. G. Faria
# Data: 21/06/2026
# Objetivo: Resolução da lista de exercícios 10

# Exercício 1 ------------------------------------------------

# Carregar pacotes
library(dplyr)
library(tidyr)
library(tidyquant)
library(infer)

# cria o objeto do experimento
dados_incentivos <- 
  tibble(
    id = 1:40,
    grupo = c(
      rep("Incentivo monetário", 20),
      rep("Incentivo não monetário", 20)
    ),
    produtividade = c(
      #valores do grupo monetário
      14.8, 11.2, 12.8, 16.5, 12.9, 15.6, 13.6, 17.1, 14.2, 13.5,
      14.7, 15.1, 12.6, 18.2, 14.9, 13.2, 16.0, 14.4, 15.8, 13.9,
      
      #valores do grupo não monetário
      12.4, 13.1, 11.8, 14.2, 10.9, 12.5, 13.8, 11.5, 15.1, 12.6,
      11.9, 13.4, 12.1, 14.5, 11.2, 13.7, 12.8, 14.0, 10.5, 13.0
    )
  )

# converte a coluna grupo para fator
dados_incentivos <- dados_incentivos |>
  mutate(grupo = as.factor(grupo))

# define a ordem  e a quantidade de repetições
ordem_incentivos <- c("Incentivo monetário", "Incentivo não monetário")
n_reps <- 1000

# calcula a estatítica observada
obs_incentivos <- dados_incentivos |>
  specify(produtividade ~ grupo) |>
  calculate(stat = "diff in means", order = ordem_incentivos)

# fixa a semente para reproduzir as reamostragens
set.seed(2026)

# gera a distribuição bootstrap da diferença de médias
boot_incentivos <- dados_incentivos |>
  specify(produtividade ~ grupo) |>
  generate(reps = 1000, type = "bootstrap") |>
  calculate(stat = "diff in means", order = ordem_incentivos)

# calcula o intervalo de confiança pelo método percentil
ic_boot_incentivos <- boot_incentivos |>
  get_confidence_interval(level = 0.95, type = "percentile")

# calcula o erro-padrão
erro_padrao_ex1 <- boot_incentivos |>
  summarise(se= sd(stat))

# reune a estimativa original, o erro padrão e os limites do intervalo
tibble(
  estimativa_original = obs_incentivos$stat,
  erro_padrao         = erro_padrao_ex1$se,
  limite_inferior     = ic_boot_incentivos$lower_ci,
  limite_superior     = ic_boot_incentivos$upper_ci
)

# verifica as médias brutas
dados_incentivos |> 
  group_by(grupo) |> 
  summarise(media = mean(produtividade))

# interpretação
# O intervalo de confiança de 95% por bootstrap indica a faixa de valores 
# para a verdadeira diferença média de produtividade entre os dois tipos
# de incetivo na população, capturando a incerteza da estimativa com base
# em 1000 reamostragens.

# Exercício 2 ------------------------------------------------

# fixa a semente para reproduzir as permutações
set.seed(2026)

# gera a distribuição nula por permutanção
dist_nula_incentivos <- dados_incentivos |>
  specify(produtividade ~ grupo) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "diff in means", order = ordem_incentivos)

# calcula o valor-p bilateral
  valor_p <- dist_nula_incentivos |>
  get_p_value(obs_stat = obs_incentivos, direction = "two-sided")

print(valor_p)

# Exercício 3 ------------------------------------------------

# Ajustar o modelo linear conforme a página 89
modelo_ab <- lm(tempo_min ~ versao, data = dados_ab)

# Exibir os coeficientes e erros-padrão
summary(modelo_ab)
