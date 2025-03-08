# Carregar os pacotes necessários
rm(list = ls())
options(scipen = 999)
library(readxl) 
library(tidyr)
library(dplyr)
library(urca)
library(readr)
library(data.table)
library(moments)
library(tseries)


data <- read_excel("~/FINANCAS/DADOS/DATA_COMMODITIES.xlsx")
data <- data[, c(2, 3, 4, 5, 6, 7, 8, 9)]

for (col in names(data)) {
  data[[col]] <- as.numeric(gsub(",", ".", data[[col]]))
}

calcular_estatisticas <- function(coluna) {
  media <- mean(coluna, na.rm = TRUE)
  mediana <- median(coluna, na.rm = TRUE)
  desvio_padrao <- sd(coluna, na.rm = TRUE)
  variancia <- var(coluna, na.rm = TRUE)
  assimetria <- skewness(coluna, na.rm = TRUE)
  curtose <- kurtosis(coluna, na.rm = TRUE)
  minimo <- min(coluna, na.rm = TRUE)
  maximo <- max(coluna, na.rm = TRUE)
  jarque_bera <- jarque.bera.test(coluna)
  p_valor_jb <- jarque_bera$statistic
  
  c(media, mediana, desvio_padrao, variancia, assimetria, curtose, minimo, maximo, p_valor_jb)
}

# Supondo que 'data' seja seu data frame
colunas_desejadas <- c(names(data)) # Substitua pelos nomes reais das suas colunas

lista_resultados <- lapply(data[colunas_desejadas], calcular_estatisticas)

# Criar o data frame final
tabela_final <- data.frame(
  Estatistica = c("Media", "Mediana", "Desvio Padrao", "Variancia", "Assimetria", "Curtose", "Minimo", "Maximo", "Jarque-Bera")
)

# Adicionar os valores das colunas
for (i in seq_along(colunas_desejadas)) {
  tabela_final[[colunas_desejadas[i]]] <- lista_resultados[[i]]
}

# Exibir a tabela
print(tabela_final)
