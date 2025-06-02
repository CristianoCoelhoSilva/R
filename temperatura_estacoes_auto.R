library(dplyr)
library(lubridate)
library(dplyr)
library(lubridate)

temperaturaAuto <- read_csv("TEMPERATURA/TEMPERATURA/AUTOMATICAS/2023.csv")

temperaturaAuto <- temperaturaAuto %>%
  mutate(
    DATA = ymd(DATA), # Garante que DATA é um objeto de data
    Mes = format(DATA, "%B") # %B retorna o nome completo do mês
  )

temperaturaAuto <- temperaturaAuto[c(1,4,5,6,2,3)]

temperaturaAuto$Mes <- toupper(temperaturaAuto$Mes)

estacoesAuto <-  read.delim("TEMPERATURA/ESTACOES/CatalogoEstaçõesAutomáticas.csv", sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")

temperaturaAuto <- temperaturaAuto %>%
  inner_join(estacoesAuto, by = c("CODIGO_ESTACAO" = "CD_ESTACAO"))

temperaturaAuto <- temperaturaAuto[c(1,2,7,3,4,5,6)]

temperaturaAuto$temperatura_maxima[is.infinite(temperaturaAuto$temperatura_maxima)] <- NA
temperaturaAuto$temperatura_minima[is.infinite(temperaturaAuto$temperatura_minima)] <- NA

##Remover temperaturaAutos onde não tem a minima e nem a maxima
temperaturaAuto <- temperaturaAuto %>%
  filter(!is.na(temperatura_maxima) | !is.na(temperatura_minima))

temperaturaAuto <- temperaturaAuto %>%
  inner_join(normal_pivot, by = c("CODIGO_ESTACAO" = "CD_ESTACAO", "Mes" = "Mes"))

temperaturaAuto <- temperaturaAuto[c(2,10,1,5,6,7,11,12)]

#write_xlsx(temperaturaAuto, path = "temperaturaAuto/dados_vakudar.xlsx")

# Substituir vírgulas por pontos e converter para numérico
temperaturaAuto$temperatura_maxima <- as.numeric(gsub(",", ".", temperaturaAuto$temperatura_maxima))
temperaturaAuto$temperatura_minima <- as.numeric(gsub(",", ".", temperaturaAuto$temperatura_minima))
temperaturaAuto$Max <- as.numeric(gsub(",", ".", temperaturaAuto$Max))
temperaturaAuto$Min <- as.numeric(gsub(",", ".", temperaturaAuto$Min))