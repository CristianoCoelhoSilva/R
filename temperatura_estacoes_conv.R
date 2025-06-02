library(dplyr)
library(lubridate)
library(dplyr)
library(lubridate)

temperaturaConv <- read_csv("TEMPERATURA/TEMPERATURA/CONVENCIONAIS/2023.csv")

temperaturaConv <- temperaturaConv %>%
  mutate(
    DATA = ymd(Data), # Garante que DATA é um objeto de data
    Mes = format(Data, "%B") # %B retorna o nome completo do mês
  )

temperaturaConv <- temperaturaConv[c(5,1,6,3,4)]

temperaturaConv <- temperaturaConv %>%
  rename(
    CODIGO_ESTACAO = arquivo
  )

temperaturaConv$CODIGO_ESTACAO <- sub("dados_([0-9]+)_D.*", "\\1", temperaturaConv$CODIGO_ESTACAO)

temperaturaConv$Mes <- toupper(temperaturaConv$Mes)

temperaturaConv <- temperaturaConv %>%
  mutate(CODIGO_ESTACAO = as.integer(CODIGO_ESTACAO))

estacoesConv <-  read.delim("TEMPERATURA/ESTACOES/CatalogoEstaçõesConvencionais.csv", sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")

temperaturaConv <- temperaturaConv %>%
  inner_join(estacoesConv, by = c("CODIGO_ESTACAO" = "CD_ESTACAO"))

temperaturaConv <- temperaturaConv[c(1,7,6,2,3,4,5)]

temperaturaConv$temperatura_maxima[is.infinite(temperaturaConv$temperatura_maxima)] <- NA
temperaturaConv$temperatura_minima[is.infinite(temperaturaConv$temperatura_minima)] <- NA

##Remover temperaturas onde não tem a minima e nem a maximadados_sem_nulos <- dados_temperatura %>%
temperaturaConv <- temperaturaConv %>%
  filter(!is.na(temperatura_maxima) | !is.na(temperatura_minima))

temperaturaConv <- temperaturaConv %>%
  mutate(CODIGO_ESTACAO = as.character(CODIGO_ESTACAO))

temperaturaConv <- temperaturaConv %>%
  rename(
    ESTADO = SG_ESTADO
  )

temperaturaConv <- temperaturaConv %>%
  inner_join(normal_pivot, by = c("CODIGO_ESTACAO" = "CD_ESTACAO", "Mes" = "Mes"))

temperaturaConv <- temperaturaConv[c(2,10,1,5,6,7,11,12)]

#write_xlsx(temperatura, path = "TEMPERATURA/dados_vakudar.xlsx")

# Substituir vírgulas por pontos e converter para numérico
temperaturaConv$temperatura_maxima <- as.numeric(gsub(",", ".", temperaturaConv$temperatura_maxima))
temperaturaConv$temperatura_minima <- as.numeric(gsub(",", ".", temperaturaConv$temperatura_minima))
temperaturaConv$Max <- as.numeric(gsub(",", ".", temperaturaConv$Max))
temperaturaConv$Min <- as.numeric(gsub(",", ".", temperaturaConv$Min))

