# Carrega os pacotes necessários
library(readr)
library(dplyr)
library(lubridate)
library(readxl)
library(writexl)
library(tidyr)
library(geobr)
library(sf)


source('TEMPERATURA/TEMPERATURA_CODES/Normais_estacoes.R')
source('TEMPERATURA/TEMPERATURA_CODES/temperatura_estacoes_auto.R')
source('TEMPERATURA/TEMPERATURA_CODES/temperatura_estacoes_conv.R')
source('TEMPERATURA/TEMPERATURA_CODES/temperatura_final.R')
source('TEMPERATURA/TEMPERATURA_CODES/Union_bases.R')


anos_para_processar <- 2008:2019


normal_pivot_data <- processa_dados_temperatura(base_path = '') # Ajuste base_path se necessário

for (ano_atual in anos_para_processar) {
  cat(paste0("\nIniciando processamento para o ano: ", ano_atual, "\n"))
  
  tryCatch({

    temperaturaAuto <- processa_temperatura_auto(ano_atual, normal_pivot_data) 
    cat(paste0("Temperatura automática para ", ano_atual, " processada.\n"))
    
    temperaturaConve <- processa_temperatura_conv(ano_atual, normal_pivot_data) 
    cat(paste0("Temperatura automática para ", ano_atual, " processada.\n"))
    
    temperatura_com_anomalias <- calcula_anomalias_temperatura(temperaturaAuto, temperaturaConve)
    cat(paste0("Anomalias de temperatura para ", ano_atual, " calculadas.\n"))
    
    arquivo_mortalidade_ano <- paste0('FINAL', ano_atual, '.csv')
    # O path_mortalidade se refere ao diretório TEMPERATURA/MORTALIDADE/
    base_final_ano <- combina_mortalidade_temperatura(
      arquivo_mortalidade = arquivo_mortalidade_ano,
      temperatura = temperatura_com_anomalias,
      path_mortalidade = "TEMPERATURA/MORTALIDADE/" # Ajuste path_mortalidade se necessário
    )
    cat(paste0("Dados de mortalidade combinados com temperatura para ", ano_atual, ".\n"))
    
    # 4. Salva o dataframe resultante em um arquivo CSV específico para o ano
    output_filename <- paste0("TEMPERATURA/TEMPERATURA/BASE", ano_atual, ".csv")
    write_csv(base_final_ano, output_filename)
    cat(paste0("Dados finais para ", ano_atual, " salvos em: ", output_filename, "\n"))
    
  }, error = function(e) {
    cat(paste0("Erro ao processar o ano ", ano_atual, ": ", e$message, "\n"))
  })
}

cat("\nProcessamento de todos os anos concluído.\n")