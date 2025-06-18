library(readr)
library(dplyr)
library(lubridate) # Necessário para operações com datas

#' @title Combina dados de mortalidade com dados de temperatura
#' @description Esta função lê um arquivo CSV de dados de mortalidade, formata as datas,
#'   ajusta os códigos IBGE e faz a junção com os dados de temperatura previamente processados.
#' @param arquivo_mortalidade O nome do arquivo CSV contendo os dados de mortalidade (ex: "FINAL2019.csv").
#' @param temperatura Um dataframe contendo os dados de temperatura processados,
#'   incluindo as colunas `Codigo_IBGE` e `DATA`, como gerado pela função `calcula_anomalias_temperatura()`.
#' @param path_mortalidade O caminho para o diretório onde o arquivo de mortalidade está localizado
#'   (padrão: "TEMPERATURA/MORTALIDADE/").
#' @return Um dataframe `death` resultante da junção dos dados de mortalidade e temperatura.
#' @examples
#' # Assumindo que 'dados_finais_temperatura' foi gerado pela função anterior:
#' # dados_completos <- combina_mortalidade_temperatura(
#' #   arquivo_mortalidade = "FINAL2019.csv",
#' #   temperatura = dados_finais_temperatura
#' # )
#'
#' # Se o arquivo de mortalidade estiver em um diretório diferente:
#' # dados_completos <- combina_mortalidade_temperatura(
#' #   arquivo_mortalidade = "FINAL2019.csv",
#' #   temperatura = dados_finais_temperatura,
#' #   path_mortalidade = "C:/Users/SeuUsuario/Dados/Mortalidade/"
#' # )
combina_mortalidade_temperatura <- function(arquivo_mortalidade, temperatura, path_mortalidade = "TEMPERATURA/MORTALIDADE/") {
  
  # Concatena o caminho do diretório e o nome do arquivo para ler os dados de mortalidade
  death <- read_csv(paste0(path_mortalidade, arquivo_mortalidade))
  
  # Ajusta e converte a coluna DTOBITO para formato de data
  indices_7_digitos <- nchar(as.character(death$DTOBITO)) == 7
  death$DTOBITO[indices_7_digitos] <- paste0("0", death$DTOBITO[indices_7_digitos])
  death$DTOBITO <- as.Date(as.character(death$DTOBITO), format = "%d%m%Y")
  
  # Ajusta o Código IBGE no dataframe de temperatura para 6 dígitos
  temperatura$Codigo_IBGE <- substr(temperatura$Codigo_IBGE, 1, 6)
  
  # Realiza a junção interna entre os dados de mortalidade e temperatura
  # Converte Codigo_IBGE de temperatura para double para a junção,
  # garantindo que ambos os lados tenham o mesmo tipo para a chave de junção.
  base <- death %>%
    inner_join(temperatura %>% mutate(Codigo_IBGE = as.double(Codigo_IBGE)),
               by = c("CODMUNRES" = "Codigo_IBGE", "DTOBITO" = "DATA"))
  
  return(base)
}