# Carregar os pacotes necessários
rm(list = ls())
options(scipen = 999)
library(readxl) 
library(tidyr)
library(dplyr)
library(urca)

dados <- read_excel('DADOS/setores_brasil.xlsx')

dados <- dados %>%
  pivot_longer(cols = starts_with("MXBR"),  # Seleciona as colunas que começam com "MXBR"
               names_to = "Indice",         # Cria a coluna "Indice" com o nome das colunas antigas
               values_to = "Valor")        # Cria a coluna "Valor" com os valores

dados$Data <- as.Date(dados$dates, format = "%Y-%m-%d")
dados$Valor <- as.numeric(gsub(",", ".", dados$Valor))

dados <- subset(dados, Indice == 'MXBR0FN Index')

y = dados$Valor

y = diff(y)

plot.ts(y)

adf <- ur.df(y = y, type = 'trend', lags = 0, selectlags = 'BIC')

summary(adf)