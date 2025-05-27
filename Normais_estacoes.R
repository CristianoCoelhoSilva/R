library(readr)
library(readxl)
#library(sqldf)
library(dplyr)

estacoesConv <-  read.delim("TEMPERATURA/ESTACOES/CatalogoEstaçõesConvencionais.csv", sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")

estacoesAuto <-  read.delim("TEMPERATURA/ESTACOES/CatalogoEstaçõesAutomáticas.csv", sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")

Normal_TMIN <- read_excel("TEMPERATURA/NORMAIS/Normal-Climatologica-TMIN.xlsx")

Normal_TMAX <- read_excel("TEMPERATURA/NORMAIS/Normal-Climatologica-TMAX.xlsx")

Base_Conv_MAX <- inner_join(estacoesConv, Normal_TMAX, by = c("CD_ESTACAO" = "Código"))

Base_Auto_MAX <- inner_join(estacoesAuto, Normal_TMAX, by = c("SG_ESTADO" = "UF", "DC_NOME" = "Nome da Estação"))

#estacoes <- inner_join(estacoesConv, estacoesAuto, by = c("SG_ESTADO" = "SG_ESTADO", "DC_NOME" = "DC_NOME"))