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

lista_range_ano <- function(inicio, fim) {
  return(inicio:fim)
}

# Define o intervalo de anos
anos <- lista_range_ano(2020, 2023)


for (ano in anos) {
  
ano_desejado <- ano

nome_arquivo_base <- paste0("~/PROJETO/DADOS/MORTALIDADE/FINAL", ano_desejado, ".csv")
base <- read_csv(nome_arquivo_base)

indices_7_digitos <- nchar(as.character(base$DTOBITO)) == 7
base$DTOBITO[indices_7_digitos] <- paste0("0", base$DTOBITO[indices_7_digitos])
base$DTOBITO <- as.Date(as.character(base$DTOBITO), format = "%d%m%Y")

nome_arquivo_temperatura <- paste0("TEMPERATURA/DADOS/ARQUIVOS/", ano_desejado, ".csv")
dados <- read_csv(nome_arquivo_temperatura)
dados <- dados %>%
  mutate(temperatura_maxima = na_if(temperatura_maxima, -9999),
         temperatura_maxima = na_if(temperatura_maxima, Inf),
         temperatura_maxima = na_if(temperatura_maxima, -Inf))

dados <- dados %>%
  mutate(temperatura_minima = na_if(temperatura_minima, -9999),
         temperatura_minima = na_if(temperatura_minima, Inf),
         temperatura_minima = na_if(temperatura_minima, -Inf))

dados <- dados %>%
filter(!is.na(temperatura_maxima), !is.na(temperatura_minima))

estacoes <- read_csv("TEMPERATURA/DADOS/estacoes.csv")

dados <- left_join(dados, estacoes, by = c("CODIGO_ESTACAO" = "id_estacao"))

dados <- dados[c(1,2,3,4,5,6,7)]

dados$ibge = floor(dados$id / 10)

basefinal <- inner_join(dados, base, by = c("ibge" = "CODMUNRES", "DATA" = "DTOBITO"), relationship = "many-to-many")

nome_arquivo_saida <- paste0('TEMPERATURA/DADOS/BASE_FINAL_TEMPE_MORTES', ano_desejado, '.csv')
write.csv(basefinal, file = nome_arquivo_saida, row.names = FALSE, quote = TRUE, na = "")

}
