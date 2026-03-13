#=========================================================
# Disciplina: introdução à Cinência de Dados
#=========================================================
# Arquivo: 01-introducao.R
# Autor: Marden José Guimarães Faria
# Data: 12/03/2026
# Obetivo: Aprender a usar Rstudio, script R e alguns fundamentos da linguagem R

# Atalho para criar seções de código: Crtl + Shift + R


# Configurações globais ---------------------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necesários
library(here) # para usar caminhos relativos
library(tidyverse) # meta-pacote que inclui readr, dplyr, ggplot2...
library(skimr) # para compreender os dados
library(janitor) # para limpar nomes de colunas


# R como uma grande calculadora -------------------------------------------

# Operações aritmeticas básicas

# adição
15 + 7

# subtração
20-6

# multiplicação
8 * 9

## divisão
84 / 7

## potenciação
2^5

# Predência de operações matemáticas
# parenteses primeiro, depois potenciação, multiplicação e divisão
# e por último adição e subtração

# parentese primeiro
(15+7) * 2
84 / (7+5)


# Exemplos de funções matemáticas -----------------------------------------

# logaritmo natural
log(100)

# logaritmo base 10
log10(100)

# função exponencial (e elevado a x)
exp(1)

# valor absoluto
abs (-45)

# raiz quadrada
sqrt(225)

# arredondamento para duas casas decimais
round(3.14159, digits = 2)


# Tipos de atômicos e classes ---------------------------------------------

# Os tipos de dados definem como os dados são armazenados na memória

# tipo double e classe numeric
a <- 3.14
a
# função que retorna o tipo do objeto
typeof(a)
# função que retorna a classe do objeto
class (a)

# character 
b <- "João"
b

# logical
c <- TRUE
c

d <- FALSE
d

# NaN (Not a Number) representa um valor indefinido
e <- 0 / 0
e

# Inf (Infinity) representa um valor indefinido
f <- 1 / 0
f

# coerção de loginal para numeric
# TRUE = 1 E FALSE = 0
f <- as.numeric(c)
f



# Vetores numéricos -------------------------------------------------------

# Cria dos vetores numericos com dados de receita e custos diários

receita_diária <- c(9200, 8700, 10100, 9800, 11050)
print(receita_diária)

custo_diário <- c(6400, 6000, 7200, 6800, 7600)
custo_diário

# Vetorização: operações elemento a elemento
lucro_diário <- receita_diária - custo_diário
margem_diaria <- lucro_diário - receita_diária


# Funções úteis para vetores numéricos ------------------------------------

# logaritmo da receita diária
log(receita_diária)

# receita total da semana
sum(receita_diária)

#receita média da semana
mean(receita_diária)

# tamanho do vetor de receita
# Nesse caso é o número de dias registrados
length(receita_diária)

#receita mínima da semanda
min(receita_diária)

# receita máxima da semnana
max(receita_diária)

# vendo a ajuda de uma função
?mean
?length


# Vetores de caracteres lógicos -------------------------------------------

# vetores devem ter o mesmo tipo de dados, ou seja,
# todos os elementos devem ser do mesmo tipo

# vetor de cacteres
nome_empresa <- c("Loja A", "Loja B", "Loja C")

# vetor lógico (booleano) indicado se a meta de vendas foi batida
meta_batida  <- c(TRUE, FALSE, TRUE)

# exibe o vetor criado
meta_batida


# Fatores -----------------------------------------------------------------


# Fatores são usados para amenizar variáveis categóricas
# nominais ou ordinárias  

# Vetor de caracteres com meses do ano
meses <- c("Dez","Abr","Jan","Mar")
meses

# um vetor de caracretes é ordenado por ordem alfabética
sort(meses)

# definindo os níveis dos meses em ordem cronólogica
niveis_meses <- c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
  "Jul", "Ago", "Set", "Out", "Nov", "Dez"
)

# converet o vetor meses para o fator, usando os níveis definidos
meses  <- factor(meses, levels = niveis_meses)

# ordena os meses
sort(meses)


# Importa os arquivos de dados---------------------------------------------

# define o caminho relativo para o arqivo csv 
# usando a função here() do pacote here
caminho_csv <- here("data/raw/dados_vendas.csv")

# importa o arquivo csv com a função readr do pacote readr
# e armazena no objeto dados_vendas
dados_vendas <- read_csv(caminho_csv)


# Compreendendo os dados---------------------------------------------------

## exibe visão geral de dados
glimpse(dados_vendas)

## visualiza as primeiras linhas da tabela
head(dados_vendas)

## visão detalhada dos dados
skim(dados_vendas)


# Preparação dos dados para análise----------------------------------------

## limpa os nomes das colunas
dados_vendas <- dados_vendas |>
  janitor:: clean_names()

## visão geral dos dados
glimpse(dados_vendas)

## converter as colunas de cidade, representante e produto para fatores
dados_vendas_limpos <- dados_vendas |>
  mutate(
    cidade = as.factor(cidade),
    representante = as.factor(representante),
    produto = as.factor(produto)
  )
  
## verifica a estrutura de dados
glimpse(dados_vendas_limpos)

## resumo estatístico do objeto
summary(dados_vendas_limpos)


# Salva os dados limpos--------------------------------------------------

# define o caminho relativo da pasta onde o arquivo limpo será salvo
caminho_csv_limpo <- here("data/clean/dados_vendas_limpos.rds")

# salva o objeto dados_vendas_limpos no formato rds
readr::write_rds(dados_vendas_limpos, caminho_csv_limpo)


## Que perguntas de negócios você faria para esse conjunto de dados?


# Manipulação/análise como o pacote dplyr--------------------------------

# Exemplo 1
# Pergunta de negócio: quero apenas as vendas realizadas em Formiga

dados_vendas_limpos |>
  filter(cidade == "Formiga")

# Exemplo 2
# Pergunta de negócio: quero apenas as vendas realizadas em Formiga que
# geram receita maior que 1000

dados_vendas_limpos |>
  filter(cidade == "Formiga" & receita > 1000)

# Exemplo 3
# Pergunta de negócio: quero apenas as colunas cidade e receita

dados_vendas_limpos |>
  select(cidade, receita)

# Exemplo 4
# Pergunta de negócio: quero saber a receita total por cidade
  
receita_por_cidade <- dados_vendas_limpos |>
  group_by(cidade) |>
  summarise(receita_total = sum (receita))

# exibe o código criado
receita_por_cidade

# Exemplo 5
# Pergunta de negócio: quero saber a receita total por produto

dados_vendas_limpos |>
  group_by(produto) |>
  summarise(receita_total = sum (receita))

# Exemplo 6
# Pergunta de negócio: quero saber a receita total por cidade em
# ordem decrescente e salvar o resultado em outro objeto

receita_por_cidade_produto  <- dados_vendas_limpos |>
  group_by(cidade) |>
  summarise(receita_total = sum (receita)) |>
  arrange(desc(receita_total))

# exibe o código criado
receita_por_cidade_produto

# Exemplo 7
# Pergunta de negócio: quero saber a receita total por cidade e representante
# em ordem decrescente de receita

dados_vendas_limpos |>
  group_by(cidade, representante) |>
  summarise(receita_total = sum (receita)) |>
  arrange(desc(receita_total))

# Exemplo 8
# Pergunta de negócio: quero saber a receita total por cidade e produto
# em ordem decrescente

dados_vendas_limpos |>
  group_by(cidade, produto) |>
  summarise(receita_total = sum (receita)) |>
  arrange(desc(receita_total))

# exibe o código criado
receita_por_cidade_produto


# Resolução dos exercícios---------------------------------------------

# Cria o vetor 

# Soma dos custos totais

custos_semanais <- c(5400, 6100, 5900, NA, 6300, 6000)
print(custos_semanais)
custos_semanais

# Exercício 1

# Custo total

sum(custos_semanais, na.rm = TRUE)

# Exercício 2

# Custo médio

mean(custos_semanais, na.rm = TRUE)

# Exercício 3

# Valor menor e maior

min(custos_semanais, na.rm = TRUE)
max(custos_semanais, na.rm = TRUE)

# Exercício 4

dados_vendas_limpos|>
  filter(produto == "Produto A")

# Exercício 5

dados_vendas_limpos |>
  filter(cidade == "Piuhmi" & unidades > 10)

# Exercício 6

# Total de unidades vendidas por produto

dados_vendas_limpos |> 
  group_by(produto) |> 
  summarise(total_vendido = sum(unidades, na.rm = TRUE))
  
# Nome das colunas disponpiveis 

names(dados_vendas_limpos)

# Exercício 7

# Calcule a receita (fatruramento) média por cidade

dados_vendas_limpos |> 
  group_by(cidade) |> 
  summarise(receita_media = mean(receita, na.rm = TRUE))

# Exercício 8

# Calcule a receita total por representante

dados_vendas_limpos |> 
  group_by(representante) |> 
  summarise(receita_total = sum(receita, na.rm = TRUE))

# Exercício 9

# Calcule o menor preço unitário por produto

dados_vendas_limpos |> 
  group_by(produto) |> 
  summarise(preco_unitario = min(preco_unitario, na.rm = TRUE))
 
# Exercício 10

# sem salvar o resultado
dados_vendas_limpos |>
  select(cidade, produto)

# salvando o resultado
resultado <- dados_vendas_limpos |>
  select(cidade, produto)

# ---------------------------------FIM--------------------------------#
