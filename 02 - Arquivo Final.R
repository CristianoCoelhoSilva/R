library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(purrr) # Carregando o pacote purrr

ANO <- '2019'

diretorio_origem <- paste0('TEMPERATURA/DADOS/', ANO, '/TRATADO/')

# 1. Listar apenas os arquivos com final .csv no diretório de origem
arquivos_para_mover <- list.files(diretorio_origem, pattern = "\\.csv$", full.names = TRUE)

# 2. Ler e juntar todos os arquivos CSV em um único dataframe
dados <- arquivos_para_mover %>%
  map_df(read_csv)

nome_arquivo_csv <- paste0('TEMPERATURA/DADOS/ARQUIVOS/', ANO, ".csv")

write.csv(dados, file = nome_arquivo_csv, row.names = FALSE, fileEncoding = "UTF-8")