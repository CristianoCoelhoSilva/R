library(dplyr)
library(lubridate)

estacoesAuto <-  read.delim("TEMPERATURA/ESTACOES/CatalogoEstaçõesAutomáticas.csv", sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")

base2023 <- read_csv("TEMPERATURA/TEMPERATURA/AUTOMATICAS/2023.csv")

temperatura <- base2023 %>%
  inner_join(estacoesAuto, by = c("CODIGO_ESTACAO" = "CD_ESTACAO"))


temperatura <- temperatura %>%
  mutate(
    DATA = ymd(DATA), 
    Mes = month(DATA, label = TRUE, abbr = FALSE) 
  )

temperatura <- temperatura[c(4, 6, 1, 2, 3, 13)]

temperatura$Mes <- toupper(temperatura$Mes)

temperatura <- temperatura %>%
  inner_join(normal_pivot, by = c("ESTADO" = "SG_ESTADO", "DC_NOME" = "DC_NOME", "Mes" = "Mes"))

temperatura$temperatura_maxima[is.infinite(temperatura$temperatura_maxima)] <- NA
temperatura$temperatura_minima[is.infinite(temperatura$temperatura_minima)] <- NA

temperatura$diff_max <- temperatura$temperatura_maxima - temperatura$Max
temperatura$diff_min <- temperatura$temperatura_minima - temperatura$Min

temperatura <- temperatura %>%
  mutate(anomalia_max = ifelse(diff_max >= 5, 1, 0)) %>%
  mutate(anomalia_min = ifelse(diff_min <= -5, 1, 0))

rm(list = setdiff(ls(), c("temperatura")))