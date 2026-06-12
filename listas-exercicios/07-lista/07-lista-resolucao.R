# Arquivo: 06-lista-resolucao.R
# Autor(a): Marden J. G. Faria
# Data: 14/05/2026
# Objetivo: Resolução da lista de exercícios 7 (6)

# Exercício 1 ------------------------------------------------

library(tidyverse)

lcg <- function(n, seed, a, c, m) {
  # Crie vetores vazios para guardar os valores gerados.
  x <- numeric(n)
  u <- numeric(n)
  
  # No início, o valor anterior é a semente X_0.
  x_anterior <- seed
  
  for (i in 1:n) {
    # 1. Calcule a expressão a * X_{n-1} + c.
    valor_bruto <- a * x_anterior + c
    
    # 2. Guarde em x[i] o resto da divisão por m.
    x[i] <- valor_bruto %% m
    
    # 3. Divida x[i] por m para obter u[i], um valor em [0, 1).
    u[i] <- x[i] / m
    
    # 4. O valor guardado em x[i] será usado na próxima repetição.
    x_anterior <- x[i]
  }
  
  # Organize a sequência gerada em uma tabela.
  resultado <- tibble(
    iteracao = 1:n,
    x = x,
    u = u
  )
  
  return(resultado)
}

# Teste sua função com os parâmetros do cálculo manual (gerando os 8 primeiros)
teste_lcg <- lcg(n = 8, seed = 1, a = 5, c = 3, m = 8)
print(teste_lcg)

# Teste sua função com os parâmetros do cálculo manual (gerando 20 valores)
teste_lcg <- lcg(n = 20, seed = 1, a = 5, c = 3, m = 8)
print(teste_lcg)

# Teste sua função com os parâmetros do cálculo manual (gerando 100 valores)
teste_lcg <- lcg(n = 100, seed = 1, a = 5, c = 3, m = 8)
print(teste_lcg)

# Exercício 2 ------------------------------------------------

# Item A
library(tidyverse)

# 1. Definir a função em R
f_a <- function(x) {
  x^2
}

# 2. Configurar a semente antes de gerar os valores
set.seed(20260514)

# 3. Gerar N = 1000 valores uniformes no intervalo correto [1, 3]
N <- 1000
u_a <- runif(N, min = 1, max = 3)

# 4. Calcular a estimativa de Monte Carlo usando (b - a) * mean(f(u))
a_a <- 1
b_a <- 3
estimativa_mc_a <- (b_a - a_a) * mean(f_a(u_a))

# 5. Obter aproximação numérica de referência com integrate()
ref_a <- integrate(f_a, lower = 1, upper = 3)
valor_ref_a <- ref_a$value

# 6. Calcular o erro absoluto
erro_abs_a <- abs(estimativa_mc_a - valor_ref_a)

# Exibir os resultados de A
cat("--- ITEM A --- \n")
cat("Estimativa de Monte Carlo:", estimativa_mc_a, "\n")
cat("Valor de Referência (integrate):", valor_ref_a, "\n")
cat("Erro Absoluto:", erro_abs_a, "\n")

# Item B

# 1. Definir a função em R
f_b <- function(x) {
  sin(x)
}

# 2. Configurar a semente antes de gerar os valores
set.seed(20260514)

# 3. Gerar N = 1000 valores uniformes no intervalo correto [0, pi]
u_b <- runif(N, min = 0, max = pi)

# 4. Calcular a estimativa de Monte Carlo usando (b - a) * mean(f(u))
a_b <- 0
b_b <- pi
estimativa_mc_b <- (b_b - a_b) * mean(f_b(u_b))

# 5. Obter aproximação numérica de referência com integrate()
ref_b <- integrate(f_b, lower = 0, upper = pi)
valor_ref_b <- ref_b$value

# 6. Calcular o erro absoluto
erro_abs_b <- abs(estimativa_mc_b - valor_ref_b)

# Exibir os resultados de B
cat("\n--- ITEM B --- \n")
cat("Estimativa de Monte Carlo:", estimativa_mc_b, "\n")
cat("Valor de Referência (integrate):", valor_ref_b, "\n")
cat("Erro Absoluto:", erro_abs_b, "\n")

# Fim ----------------------------------------------------------------

