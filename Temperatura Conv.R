library(readr)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyr) 
library(writexl)
library(readr)
library(dplyr)
library(purrr) # Para a função map_df

pasta_de_arquivos <- 'TEMPERATURA/TEMPERATURA/CONVENCIONAIS'

todos_os_arquivos <- list.files(path = pasta_de_arquivos, pattern = "\\.csv$", full.names = TRUE)

temperatura <- map_df(todos_os_arquivos, read_csv)

temperatura$CODIGO_ESTACAO <- sub("dados_([0-9]+)_D.*", "\\1", temperatura$arquivo)

temperatura$arquivo <- NULL

temperatura <- temperatura %>%
  mutate(
    Mes = format(Data, "%B"), 
    Ano = format(Data, "%Y") 
  )

temperatura <- temperatura %>%
  mutate(
    DIA_MES = format(Data, "%d-%m")
  )

temperatura$Mes <- toupper(temperatura$Mes)

# Converte o nome do mês para maiúsculas
temperatura$Mes <- toupper(temperatura$Mes)

# Carrega o catálogo de estações automáticas
estacoesConv <- read.delim(paste0("TEMPERATURA/ESTACOES/CatalogoEstações.csv"),
                           sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")


estacoesConv$OLD_NAME <- estacoesConv$DC_NOME
estacoesConv$DC_NOME <- gsub("SAO PAULO\\(MIR,de SANTANA\\)|SAO PAULO - MIRANTE", "SAO PAULO", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- gsub("SAO GABRIEL DA CACHOEIRA\\(UAUPES\\)", "SAO GABRIEL DA CACHOEIRA", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- gsub("SERIDO \\(CAICO\\)", "CAICO", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- gsub("SALVADOR \\(ONDINA\\)", "SALVADOR", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- gsub("BOM JESUS DO PIAUI", "BOM JESUS", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- gsub("RIO DE JANEIRO - FORTE DE COPACABANA|RIO DE JANEIRO - JACAREPAGUA|RIO DE JANEIRO - VILA MILITAR|RIO DE JANEIRO-MARAMBAIA", "RIO DE JANEIRO", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- gsub("AGUAS EMENDADAS|BRAZLANDIA|GAMA \\(PONTE ALTA\\)|PARANOA \\(COOPA-DF\\)", "BRASILIA", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- gsub("BELO HORIZONTE - PAMPULHA|BELO HORIZONTE - CERCADINHO", "BELO HORIZONTE", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- gsub("PORTO ALEGRE - JARDIM BOTANICO", "PORTO ALEGRE", estacoesConv$DC_NOME)

temperatura <- temperatura %>%
  left_join(estacoesConv %>%
               mutate(CD_ESTACAO = as.character(CD_ESTACAO)),
             by = c("CODIGO_ESTACAO" = "CD_ESTACAO"))

temperatura <- temperatura[c(1, 5, 6, 7, 9, 2, 8)]

temperatura$temperatura_maxima[is.infinite(temperatura$temperatura_maxima)] <- NA

temperatura$temperatura_maxima[temperatura$temperatura_maxima == -9999] <- NA

temperatura <- temperatura %>% filter(!is.na(DC_NOME))

temperatura <- temperatura %>% filter(!is.na(temperatura_maxima))

temperatura$temperatura_maxima <- as.numeric(gsub(",", ".", temperatura$temperatura_maxima))

names(temperatura) <- c("DATA","Mes", "Ano", "DIA_MES", "ESTADO", "temperatura_maxima", "DC_NOME")

temperaturaconv <- temperatura

rm(list = setdiff(ls(), "temperaturaconv"))

