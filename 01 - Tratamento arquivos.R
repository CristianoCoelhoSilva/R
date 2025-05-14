library(readr)
library(tidyr)
library(dplyr)
library(lubridate)

ANO <- '2008'

diretorio_origem <- paste0('TEMPERATURA/DADOS/', ANO)

# 1. Listar apenas os arquivos com final .csv no diretório de origem
arquivos_para_mover <- list.files(diretorio_origem, pattern = "\\CSV$", full.names = TRUE)

  for (caminho_arquivo in arquivos_para_mover) {
    
    # Tenta ler o arquivo CSV
    dados <- tryCatch({
      read.delim(caminho_arquivo, skip = 8, sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")
    }, error = function(e) {
      # Se houver um erro na leitura, imprime uma mensagem e retorna NULL
      cat("Erro ao ler o arquivo:", caminho_arquivo, "\n", "Erro:", e$message, "\n")
      return(NULL) # Retorna NULL para indicar falha na leitura
    })
    
    # Verifica se a leitura do arquivo foi bem-sucedida
    if (is.null(dados)) {
      next # Pula para o próximo arquivo se a leitura falhou
    }

  dados <- dados[c(1,2,8,9,10,11,12,13)]
  
  names(dados) <- c("DATA", "HORA", "TEMPERATURA_AR_BULBO_SECO",
                    "TEMPERATURA_PONTO_ORVALHO", "TEMPERATURA_MAXIMA",
                    "TEMPERATURA_MINIMA", "TEMPERATURA_ORVALHO_MAX",
                    "TEMPERATURA_ORVALHO_MIN")
  
  dados <- unite(dados, data_hora, DATA, HORA, sep = " ", remove = FALSE)
  
  dados <- dados %>%
    mutate(data_hora = as.POSIXct(data_hora, format = "%Y/%m/%d %H%M UTC"))

  dados <- dados %>%
    group_by(DATA) %>%
    summarise(
      temperatura_maxima = max(TEMPERATURA_MAXIMA, na.rm = TRUE),
      temperatura_minima = min(TEMPERATURA_MINIMA, na.rm = TRUE)
    )
  
  partes <- strsplit(caminho_arquivo, "_")[[1]]
  
  estado <- partes[3]
  codigo_estacao <- partes[4]
  
  dados <- dados %>%
    mutate(ESTADO = estado, CODIGO_ESTACAO = codigo_estacao)
  
  nome_arquivo_csv <- paste0('TEMPERATURA/DADOS/', ANO,'/TRATADO/', estado, '_' , codigo_estacao, ".csv")
  
  write.csv(dados, file = nome_arquivo_csv, row.names = FALSE, fileEncoding = "UTF-8")
  
}