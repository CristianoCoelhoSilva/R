# Carrega os pacotes necessários
library(readr)
library(dplyr)
library(lubridate)
library(readxl)
library(writexl)
library(tidyr)
library(geobr) # Necessário para read_municipality se for usado, mas o código original usa excel.
library(sf)    # Necessário para st_drop_geometry se for usado, mas o código original usa excel.

# Carrega as funções R previamente definidas
# Certifique-se de que o caminho para esses arquivos esteja correto em seu ambiente
source('TEMPERATURA/TEMPERATURA_CODES/Normais_estacoes.R')
source('TEMPERATURA/TEMPERATURA_CODES/temperatura_estacoes_auto.R')
source('TEMPERATURA/TEMPERATURA_CODES/temperatura_final.R')
source('TEMPERATURA/TEMPERATURA_CODES/Union_bases.R')

# Define o intervalo de anos para processamento
anos_para_processar <- 2008:2019

# O dataframe 'normal' (normais climatológicas) é carregado uma única vez
# pois é o mesmo para todos os anos.
# Se seus arquivos de normais estiverem em um caminho base específico, ajuste o argumento base_path.
cat("Processando normais climatológicas...\n")
normal_pivot_data <- processa_dados_temperatura(base_path = '') # Ajuste base_path se necessário
cat("Normais climatológicas processadas com sucesso.\n")

# Loop através de cada ano para processar e salvar os dados
for (ano_atual in anos_para_processar) {
  cat(paste0("\nIniciando processamento para o ano: ", ano_atual, "\n"))
  
  tryCatch({
    # 1. Processa os dados de temperatura automática para o ano atual
    # O base_path aqui se refere ao diretório onde estão as pastas TEMPERATURA/TEMPERATURA/AUTOMATICAS/
    temperaturaAuto <- processa_temperatura_anual(ano_atual, normal_pivot_data, base_path = '') # Ajuste base_path se necessário
    cat(paste0("Temperatura automática para ", ano_atual, " processada.\n"))
    
    # 2. Calcula anomalias de temperatura
    temperatura_com_anomalias <- calcula_anomalias_temperatura(temperaturaAuto)
    cat(paste0("Anomalias de temperatura para ", ano_atual, " calculadas.\n"))
    
    # 3. Combina dados de mortalidade com dados de temperatura
    # O nome do arquivo de mortalidade deve ser dinâmico para cada ano
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
    # Você pode adicionar aqui um log ou outras ações de tratamento de erro
  })
}

cat("\nProcessamento de todos os anos concluído.\n")