# ============================================================
# Disciplina: Introdução à Ciência de Dados
# ============================================================
# Arquivo: 02-lista-resolucao.R
# Autor(a): Marden José Guimarães Faria
# Data: 19/03/2026
# Objetivo: Resolução da lista de exercícios 2

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminho relativo
library(tidyverse) # meta-pacote que inclui readr, dplyr..
library(gapminder) # contém os dados gapminder

# carrega os dados do pacote gapminder
data(gapminder)
glimpse(gapminder)
dplyr::glimpse(gapminder)


## Exercícios --------------------------------------------------------------

## Exercício 1

# Importa os arquivos de dados

# define o caminho relativo para o arqivo csv 
# usando a função here() do pacote here

caminho_csv <- here("data/raw/productionlog_sample.csv")

# importa o arquivo csv com a função readr do pacote readr
# e armazena no objeto productionlog_sample

productionlog_sample <- read_csv(caminho_csv)

# Inspecionando objeto

## exibe visão geral de dados
glimpse(productionlog_sample)

## Exercício 2

# Complete o código abaixo para selecionar apenas 
# o país, o ano e a expectativa de vida

dados_expectativa <- gapminder |>
  select(country, year, lifeExp)

## Exercício 3

# Escreva o código para selecionar todas as variáveis 
# EXCETO população e PIB per capita.

dados_exceto_variaveis_populacao_PIB <- gapminder |>
  select(-pop, -gdpPercap)

## Exercício 4

# Complete o código abaixo para selecionar apenas 
# as variáveis que começam com a letra “c”:

variveis_com_c <- gapminder |> 
  select(starts_with("C"))

## Exercício 5

# Escreva o código para selecionar todas as variáveis 
# desde country até pop (em sequência na tabela).

seleção_todas_variaveis <-  gapminder |> 
  select(country:pop)

## Exercício 6

# Crie um código que selecione variáveis usando dois métodos 
# diferentes na mesma instrução:
  
# Todas as variáveis que contêm a letra “p” OU
# Todas as variáveis que terminam com “p”

variveis_comecam_p <- gapminder |> 
  select(starts_with("p") & ends_with("p"))

## Exercício 7

# Complete o código abaixo para filtrar apenas os países 
# do continente Americas no ano de 2007.

paises_america_2007 <- gapminder |> 
  filter(continent == "americas" & year == 2007)

## Exercício 8

# Filtre a data frame gapminder para mostrar apenas os 
# dados do Brasil (country == "Brazil") e salve o resultado em um objeto

dados_brazil <- gapminder |> 
  filter(country == "Brazil")

# define o caminho relativo da pasta onde o arquivo será salvo
caminho_csv_dados_brazil <- here("data/brazil/dados_brazil.rds")

# salva o objeto no formato rds
readr::write_rds(dados_brazil, caminho_csv_dados_brazil)

## Exercício 9

# Crie um filtro para encontrar países que atendam a 
# TODAS estas condições:
  
# Pertencem ao continente Asia
# Têm população acima de 50 milhões
# Dados do ano 2007

vaiaveis_selecionadas <- gapminder |> 
  filter(continent == "Asia", 
         pop > 50000000, 
         year == 2007)

## Exercício 10

# Encontre países com expectativa de vida acima de 75 anos, 
# mas PIB per capita abaixo de 10.000 dólares em 2007.

dados_expectativa_acima_75 <- gapminder |> 
  filter(lifeExp > 75, 
         gdpPercap < 10000, 
         year == 2007)

## Exercício 11

# Complete o código abaixo para criar uma nova variável 
# que converta a população para milhões:

gapminder |> 
  mutate(pop_em_milhoes = pop / 1000000)

## Exercício 12

# Crie uma nova variável que calcule a “receita total” 
# (PIB per capita × população) para cada país.

receita_total <- gapminder |> 
    mutate(pib_total = gdpPercap * pop)

## Exercício 13

# Usando ifelse(), crie uma variável chamada economia_grande que seja “Sim” 
# quando a população for maior que 50 milhões e “Não” caso contrário

# Criando a variável economia_grande

gapminder_status <- gapminder %>%
  mutate(economia_grande = ifelse(pop > 50000000, "Sim", "Não"))

## Exercício 14

# Usando dados de 2007, crie uma variável que classifique os países 
# em três categorias baseadas na expectativa de vida:
  
# “Baixa”: menos de 60 anos
# “Média”: entre 60 e 75 anos
# “Alta”: mais de 75 anos

gapminder_2007_classificado <- gapminder |> 
  filter(year == 2007) |> 
  mutate(cat_expectativa = case_when(
    lifeExp < 60 ~ "Baixa",
    lifeExp >= 60 & lifeExp <= 75 ~ "Média",
    lifeExp > 75 ~ "Alta"
  ))

## Exercício 15

# Complete o código abaixo para calcular a expectativa 
# média de vida por continente:

expectativa_por_continente <- gapminder |> 
  group_by(lifeExp) |> 
  summarise(expextativa_media = mean(lifeExp, na.rm = TRUE))
  
## Exercício 16

# Calcule a população total por continente no ano de 2007.

populacao_continente_2007 <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(populacao_total = sum(as.numeric(pop))) |> 
  arrange(desc(populacao_total))

## Exercício 17
# Imagine que cada país representa uma filial da sua empresa. 
# Crie um objeto que mostre, para cada continente:

relatorio_filiais <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(
    n_paises = n(),                                     # Número de "filiais"
    pop_total_milhoes = sum(as.numeric(pop)) / 1e6,     # Total de "pessoas"
    pib_total_bilhoes = sum(as.numeric(gdpPercap * pop))/1e9, # "Receita" total
    expectativa_media = mean(lifeExp)                   # "Índice de qualidade"
  ) %>%
  arrange(desc(pib_total_bilhoes))

## Exercício 18

# Crie um objeto que mostre a evolução da expectativa média de vida 
# do continente americano ao longo dos anos (dica: agrupe por ano, 
# filtre para mostrar apenas as Americas).

evolucao_americas <- gapminder |> 
  filter(continent == "Americas") |> 
  group_by(year) |> 
  summarise(
    expectativa_media = mean(lifeExp),
    pop_total_milhoes = sum(as.numeric(pop)) / 1000000
  ) |> 
  arrange(year)

## Exercício 19

# Complete o código abaixo para ordenar os países por expectativa
# de vida (do maior para o menor):

paises_ordenados <- gapminder |> 
  arrange(desc(lifeExp))

## Exercício 20

# Escreva um código para listar os 5 países com
# menor PIB per capita em 2007.

paises_menor_pib <- gapminder |> 
  filter(year == 2007) |> 
  arrange(gdpPercap) |> 
  head(5)

## Exercício 21

# Imagine que você trabalha no departamento internacional de uma empresa. 
# Crie uma lista dos países das Américas ordenados por população 
# (do maior para o menor) em 2007.

ranking_americas_pop <- gapminder |> 
  filter(continent == "Americas", year == 2007) |> 
  arrange(desc(pop))

## Exercício 22

# Crie um ranking dos continentes baseado na expectativa média de vida 
# de seus países em 2007. Use group_by(), summarise() e arrange().

ranking_continentes <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(expectativa_media = mean(lifeExp)) |> 
  arrange(desc(expectativa_media))


# Fim ---------------------------------------------------------------------







