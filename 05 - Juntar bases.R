rm(list = ls())
library(readr)
library(dplyr)
library(data.table)
library(bit64)
library(tidyverse)
library(stargazer)
library(lfe)
library(lubridate)
library(gridExtra)
library(grid)
library(fixest)

# Define o ano desejado
ano_desejado <- 2020 # Você pode alterar este valor para o ano que quiser

# Carrega a base de dados com o ano configurável
nome_arquivo_base <- paste0("~/PROJETO/DADOS/FINAL", ano_desejado, ".csv")
base <- read_csv(nome_arquivo_base)

# Corrige as datas com 7 dígitos
indices_7_digitos <- nchar(as.character(base$DTOBITO)) == 7
base$DTOBITO[indices_7_digitos] <- paste0("0", base$DTOBITO[indices_7_digitos])
base$DTOBITO <- as.Date(as.character(base$DTOBITO), format = "%d%m%Y")

# Define o ano para os dados de temperatura
ano_temperatura <- 2020 # Você pode alterar este valor se necessário
nome_arquivo_temperatura <- paste0("TEMPERATURA/DADOS/ARQUIVOS/", ano_temperatura, ".csv")
dados <- read_csv(nome_arquivo_temperatura)
estacoes <- read_csv("TEMPERATURA/DADOS/estacoes.csv")

# Faz o join com os dados das estações
dados <- left_join(dados, estacoes, by = c("CODIGO_ESTACAO" = "id_estacao"))

# Seleciona as colunas desejadas
dados <- dados[c(1,2,3,4,5,6,7)]

# Calcula a coluna ibge
dados$ibge = floor(dados$id / 10)

# Faz o inner join das bases
base <- inner_join(dados, base, by = c("ibge" = "CODMUNRES", "DATA" = "DTOBITO"))

# Define o nome do arquivo de saída com o ano configurável
nome_arquivo_saida <- paste0('TEMPERATURA/DADOS/BASE_FINAL_TEMPE_MORTES', ano_temperatura, '.csv')
write.csv(base, file = nome_arquivo_saida, row.names = FALSE, quote = TRUE, na = "")
