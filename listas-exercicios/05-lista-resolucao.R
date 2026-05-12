# Arquivo: 05-lista-resolucao.R
# Autor(a): Marden J. G. Faria
# Data: 09/05/2026
# Objetivo: Resolução da lista de exercícios 5


# Exercício 7 ------------------------------------------------

# 1. Definindo a semente para reprodutibilidade
set.seed(2026)

# Definindo os tamanhos das amostras
tamanhos <- c(1000, 10000, 100000)

# Criando um vetor para armazenar os resultados das frequências
frequencias <- numeric(length(tamanhos))

# Loop para realizar as simulações (Passos 2, 3 e 4)
for (i in 1:length(tamanhos)) {
  # Simula os lançamentos: 1 para "cara", 0 para "coroa"
  simulacao <- sample(c(1, 0), size = tamanhos[i], replace = TRUE, prob = c(0.5, 0.5))
  
  # Calcula a frequência relativa de caras (média dos valores 1)
  frequencias[i] <- mean(simulacao)
}

# 5. Comparação dos resultados
resultados <- data.frame(
  Lancamentos = tamanhos,
  Freq_Relativa = frequencias,
  Diferenca_Abs = abs(frequencias - 0.5)
)

print(resultados)


# Exercício 8 ------------------------------------------------

# 1. Configuração inicial
set.seed(2026)
n_empresas <- 100000

# 2. Simular quais empresas entram em dificuldade financeira (8%)
# 1 = Dificuldade, 0 = Saudável
dificuldade <- sample(c(1, 0), size = n_empresas, replace = TRUE, prob = c(0.08, 0.92))

# 3. Simular a classificação de risco elevado
risco_elevado <- numeric(n_empresas)

# Se a empresa está em dificuldade (1), a chance de risco elevado é 85%
risco_elevado[dificuldade == 1] <- sample(c(1, 0), size = sum(dificuldade == 1), 
                                          replace = TRUE, prob = c(0.85, 0.15))

# Se a empresa está saudável (0), a chance de risco elevado é 25%
risco_elevado[dificuldade == 0] <- sample(c(1, 0), size = sum(dificuldade == 0), 
                                          replace = TRUE, prob = c(0.25, 0.75))

# 4. Proporção total de empresas classificadas como risco elevado
prop_risco_total <- mean(risco_elevado)

# 5. Estimar P(dificuldade | risco elevado)
# Filtrar apenas as empresas classificadas como risco elevado e ver quantas têm dificuldade real
p_condicional_simulada <- mean(dificuldade[risco_elevado == 1])

# Exibição dos Resultados
cat("Proporção Total Risco Elevado (Simulada):", prop_risco_total, "\n")
cat("P(Dificuldade | Risco Elevado) (Simulada):", p_condicional_simulada, "\n")


#Fim-------------------------------------------------------

