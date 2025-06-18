library(readr)
library(dplyr)
library(lubridate)
library(readxl) # Necessário para read_excel se a função anterior não for chamada
library(tidyr)  # Necessário para pivot_longer/wider se a função anterior não for chamada
library(writexl) # Necessário para write_xlsx

#' @title Processa dados de temperatura automática para um ano específico
#' @description Esta função lê, limpa e combina dados de temperatura automática de um ano
#'   com as normais climatológicas, e adiciona informações de estação.
#' @param ano O ano para o qual os dados de temperatura automática serão processados.
#' @param normal_pivot Um dataframe contendo as normais climatológicas de temperatura,
#'   gerado pela função `processa_dados_temperatura()`.
#' @param base_path O caminho base para os diretórios 'TEMPERATURA/TEMPERATURA/AUTOMATICAS/',
#'   'TEMPERATURA/ESTACOES/', etc. (padrão é o diretório de trabalho atual).
#' @return Um dataframe tidy com os dados de temperatura automática combinados com as normais climatológicas.
#' @examples
#' # Supondo que 'normal_pivot' já foi gerado pela função anterior:
#' # normal_data <- processa_dados_temperatura()
#' # temperatura_2019_processada <- processa_temperatura_anual(2019, normal_data)
#'
#' # Se os arquivos estiverem em um caminho específico:
#' # temperatura_2019_processada <- processa_temperatura_anual(2019, normal_data, base_path = "C:/Dados/")
processa_temperatura_auto <- function(ano, normal_pivot) {
  
  # Carrega os dados de temperatura automática para o ano especificado
  temperaturaAuto <- read_csv(paste0("TEMPERATURA/TEMPERATURA/AUTOMATICAS/", ano, ".csv"),
                              col_types = cols(DATA = col_character())) # Lê DATA como caractere inicialmente
  
  # Converte a coluna DATA para formato de data e extrai o mês
  temperaturaAuto <- temperaturaAuto %>%
    mutate(
      DATA = ymd(DATA), # Garante que DATA é um objeto de data
      Mes = format(DATA, "%B") # %B retorna o nome completo do mês
    )
  
  # Reordena as colunas
  temperaturaAuto <- temperaturaAuto[c(1,4,5,6,2,3)]
  
  # Converte o nome do mês para maiúsculas
  temperaturaAuto$Mes <- toupper(temperaturaAuto$Mes)
  
  # Carrega o catálogo de estações automáticas
  estacoesAuto <- read.delim(paste0("TEMPERATURA/ESTACOES/CatalogoEstaçõesAutomáticas.csv"),
                             sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")
  
  # Junta os dados de temperatura com as informações das estações automáticas
  temperaturaAuto <- temperaturaAuto %>%
    inner_join(estacoesAuto, by = c("CODIGO_ESTACAO" = "CD_ESTACAO"))
  
  # Reordena as colunas após a junção
  temperaturaAuto <- temperaturaAuto[c(1,2,7,3,4,5,6)]
  
  # Substitui valores infinitos por NA nas colunas de temperatura
  temperaturaAuto$temperatura_maxima[is.infinite(temperaturaAuto$temperatura_maxima)] <- NA
  temperaturaAuto$temperatura_minima[is.infinite(temperaturaAuto$temperatura_minima)] <- NA
  
  temperaturaAuto$temperatura_maxima[temperaturaAuto$temperatura_maxima == -9999] <- NA
  temperaturaAuto$temperatura_minima[temperaturaAuto$temperatura_minima == -9999] <- NA
  
  # Remove linhas onde tanto a temperatura máxima quanto a mínima são NA
  temperaturaAuto <- temperaturaAuto %>%
    filter(!is.na(temperatura_maxima) | !is.na(temperatura_minima))
  
  # Junta os dados de temperatura com as normais climatológicas (normal_pivot)
  temperaturaAuto <- temperaturaAuto %>%
    inner_join(normal_pivot, by = c("CODIGO_ESTACAO" = "CD_ESTACAO", "Mes" = "Mes"))
  
  # Reordena as colunas finais
  temperaturaAuto <- temperaturaAuto[c(2,10,1,5,6,7,11,12)]
  
  # Substitui vírgulas por pontos e converte para numérico as colunas de temperatura
  # (Isso é feito novamente aqui, caso os dados de entrada não tenham sido limpos previamente)
  temperaturaAuto$temperatura_maxima <- as.numeric(gsub(",", ".", temperaturaAuto$temperatura_maxima))
  temperaturaAuto$temperatura_minima <- as.numeric(gsub(",", ".", temperaturaAuto$temperatura_minima))
  temperaturaAuto$Max <- as.numeric(gsub(",", ".", temperaturaAuto$Max))
  temperaturaAuto$Min <- as.numeric(gsub(",", ".", temperaturaAuto$Min))
  
  return(temperaturaAuto)
}