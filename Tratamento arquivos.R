library(readr)
library(tidyr)
library(dplyr)

caminho_arquivo <- "TEMPERATURA/DADOS/2023/INMET_S_RS_A887_CAPAO DO LEAO (PELOTAS)_01-01-2023_A_31-12-2023.CSV"

# Tente diferentes codificações com read.delim()
dados <- read.delim(caminho_arquivo, skip = 8, sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")

dados <- dados[c(1,2,8,9,10,11,12,13)]

names(dados) <- c("DATA", "HORA", "TEMPERATURA_AR_BULBO_SECO",
                  "TEMPERATURA_PONTO_ORVALHO", "TEMPERATURA_MAXIMA",
                  "TEMPERATURA_MINIMA", "TEMPERATURA_ORVALHO_MAX",
                  "TEMPERATURA_ORVALHO_MIN")

dados <- unite(dados, data_hora, DATA, HORA, sep = " ", remove = FALSE)

dados <- dados %>%
  mutate(data_hora = as.POSIXct(data_hora, format = "%Y/%m/%d %H%M UTC"))