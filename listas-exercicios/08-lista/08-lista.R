# Arquivo: 08-lista-resolucao.R
# Autor(a): Marden J. G. Faria
# Data: 21/05/2026
# Objetivo: Resolução da lista de exercícios 8

# Exercício 1 ------------------------------------------------

# fixa a semente 
set.seed(123)

# simula U ~ Uniforme(0,1)
u  <- runif(10000)

# define o parâmetro da Exponencial
mu <- 0
beta <- 1

# gera X ~ Exponencial(mu, beta)
x <- mu + beta * log(u / (1-u))    


# Exercício 2  ------------------------------------------------

# carrega o pacote
library(triangulr)

# sintaxe da função
rtri(n, a, b, c)