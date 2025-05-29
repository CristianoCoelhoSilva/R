library(dplyr)
library(lubridate)
library(dplyr)
library(lubridate)

temperatura <- read_csv("TEMPERATURA/TEMPERATURA/AUTOMATICAS/2023.csv")

temperatura <- temperatura %>%
  mutate(
    DATA = ymd(DATA), # Garante que DATA é um objeto de data
    Mes = format(DATA, "%B") # %B retorna o nome completo do mês
  )


temperatura <- temperatura[c(1,4,5,6,2,3)]

temperatura$Mes <- toupper(temperatura$Mes)

estacoesAuto <-  read.delim("TEMPERATURA/ESTACOES/CatalogoEstaçõesAutomáticas.csv", sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")

temperatura <- temperatura %>%
  inner_join(estacoesAuto, by = c("CODIGO_ESTACAO" = "CD_ESTACAO"))

temperatura <- temperatura[c(1,2,7,3,4,5,6)]

temperatura <- temperatura %>%
  inner_join(normal_pivot, by = c("CODIGO_ESTACAO" = "CD_ESTACAO", "Mes" = "Mes"))

temperatura$temperatura_maxima[is.infinite(temperatura$temperatura_maxima)] <- NA
temperatura$temperatura_minima[is.infinite(temperatura$temperatura_minima)] <- NA

temperatura <- temperatura[c(2,10,1,5,6,7,11,12)]

#write_xlsx(temperatura, path = "TEMPERATURA/dados_vakudar.xlsx")

# Substituir vírgulas por pontos e converter para numérico
temperatura$temperatura_maxima <- as.numeric(gsub(",", ".", temperatura$temperatura_maxima))
temperatura$temperatura_minima <- as.numeric(gsub(",", ".", temperatura$temperatura_minima))
temperatura$Max <- as.numeric(gsub(",", ".", temperatura$Max))
temperatura$Min <- as.numeric(gsub(",", ".", temperatura$Min))

temperatura <- temperatura %>%
  group_by(ESTADO,	Codigo_IBGE,	DATA,	Mes) %>%
  summarise(
    media_temperatura_maxima = mean(temperatura_maxima, na.rm = TRUE),
    media_temperatura_minima = mean(temperatura_minima, na.rm = TRUE),
    media_max = mean(Max, na.rm = TRUE),
    media_min = mean(Min, na.rm = TRUE)
  )

temperatura$diff_max <- temperatura$media_temperatura_maxima - temperatura$media_max
temperatura$diff_min <- temperatura$media_temperatura_minima - temperatura$media_min

temperatura <- temperatura %>%
  mutate(anomalia_max = ifelse(diff_max >= 5, 1, 0)) %>%
  mutate(anomalia_min = ifelse(diff_min <= -5, 1, 0))

rm(list = setdiff(ls(), c("temperatura")))