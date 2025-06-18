# Carrega as bibliotecas necessárias
library(dplyr)
library(lubridate)
library(readr) # Para read_csv
library(writexl) # Para write_xlsx, se descomentado

#' Processa dados de temperatura convencionais para um determinado ano.
#'
#' Esta função lê, limpa, transforma e une dados de temperatura convencionais
#' com informações de estações e dados de normalização.
#'
#' @param ano O ano para o qual os dados de temperatura devem ser processados (numérico).
#' @param normal_pivot Um dataframe contendo dados de normalização, com colunas
#'        'CD_ESTACAO' (código da estação) e 'Mes' (mês).
#'
#' @return Um dataframe processado contendo os dados de temperatura unidos
#'         e limpos, ou NULL se ocorrer um erro.
#' @examples
#' # Supondo que 'normal_pivot' já esteja carregado no seu ambiente
#' # temperatura_processada_2019 <- processar_dados_temperatura(2019, normal_pivot)
processa_temperatura_conv <- function(ano, normal_pivot) {
  # Verifica se o dataframe normal_pivot foi fornecido
  if (missing(normal_pivot) || !is.data.frame(normal_pivot)) {
    message("Erro: O dataframe 'normal_pivot' é obrigatório e não foi fornecido ou não é um dataframe.")
    return(NULL)
  }
  
  # Constrói o caminho do arquivo para os dados de temperatura convencionais
  caminho_temperatura_conv <- paste0("TEMPERATURA/TEMPERATURA/CONVENCIONAIS/", ano, ".csv")
  
  # Tenta ler o arquivo de temperatura
  temperaturaConv <- tryCatch(
    {
      read_csv(caminho_temperatura_conv, show_col_types = FALSE)
    },
    error = function(e) {
      message(paste("Erro ao ler o arquivo de temperatura:", e$message))
      return(NULL)
    }
  )
  
  if (is.null(temperaturaConv)) {
    return(NULL)
  }
  
  # Garante que DATA é um objeto de data e extrai o nome do mês
  temperaturaConv <- temperaturaConv %>%
    mutate(
      DATA = ymd(Data), # Garante que DATA é um objeto de data
      Mes = format(Data, "%B") # %B retorna o nome completo do mês
    )
  
  # Seleciona e reordena colunas
  temperaturaConv <- temperaturaConv[c(5, 1, 6, 3, 4)]
  
  # Renomeia a coluna 'arquivo' para 'CODIGO_ESTACAO'
  temperaturaConv <- temperaturaConv %>%
    rename(
      CODIGO_ESTACAO = arquivo
    )
  
  # Extrai o código da estação do nome do arquivo
  temperaturaConv$CODIGO_ESTACAO <- sub("dados_([0-9]+)_D.*", "\\1", temperaturaConv$CODIGO_ESTACAO)
  
  # Converte o nome do mês para maiúsculas
  temperaturaConv$Mes <- toupper(temperaturaConv$Mes)
  
  # Converte CODIGO_ESTACAO para inteiro
  temperaturaConv <- temperaturaConv %>%
    mutate(CODIGO_ESTACAO = as.integer(CODIGO_ESTACAO))
  
  # Constrói o caminho do arquivo para o catálogo de estações
  caminho_estacoes_conv <- "TEMPERATURA/ESTACOES/CatalogoEstaçõesConvencionais.csv"
  
  # Tenta ler o arquivo de estações
  estacoesConv <- tryCatch(
    {
      read.delim(caminho_estacoes_conv, sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")
    },
    error = function(e) {
      message(paste("Erro ao ler o arquivo de estações:", e$message))
      return(NULL)
    }
  )
  
  if (is.null(estacoesConv)) {
    return(NULL)
  }
  
  # Realiza a junção interna com o catálogo de estações
  temperaturaConv <- temperaturaConv %>%
    inner_join(estacoesConv, by = c("CODIGO_ESTACAO" = "CD_ESTACAO"))
  
  # Seleciona e reordena colunas novamente
  temperaturaConv <- temperaturaConv[c(1, 7, 6, 2, 3, 4, 5)]
  
  # Substitui valores infinitos por NA nas colunas de temperatura
  temperaturaConv$temperatura_maxima[is.infinite(temperaturaConv$temperatura_maxima)] <- NA
  temperaturaConv$temperatura_minima[is.infinite(temperaturaConv$temperatura_minima)] <- NA
  
  # Remove linhas onde tanto temperatura_maxima quanto temperatura_minima são NA
  temperaturaConv <- temperaturaConv %>%
    filter(!is.na(temperatura_maxima) | !is.na(temperatura_minima))
  
  # Converte CODIGO_ESTACAO de volta para caractere para a próxima junção
  temperaturaConv <- temperaturaConv %>%
    mutate(CODIGO_ESTACAO = as.character(CODIGO_ESTACAO))
  
  # Renomeia a coluna 'SG_ESTADO' para 'ESTADO'
  temperaturaConv <- temperaturaConv %>%
    rename(
      ESTADO = SG_ESTADO
    )
  
  # Realiza a junção interna com o dataframe normal_pivot
  temperaturaConv <- temperaturaConv %>%
    inner_join(normal_pivot, by = c("CODIGO_ESTACAO" = "CD_ESTACAO", "Mes" = "Mes"))
  
  # Seleciona e reordena as colunas finais
  temperaturaConv <- temperaturaConv[c(2, 10, 1, 5, 6, 7, 11, 12)]
  
  # Substitui vírgulas por pontos e converte para numérico para as colunas de temperatura
  temperaturaConv$temperatura_maxima <- as.numeric(gsub(",", ".", temperaturaConv$temperatura_maxima))
  temperaturaConv$temperatura_minima <- as.numeric(gsub(",", ".", temperaturaConv$temperatura_minima))
  temperaturaConv$Max <- as.numeric(gsub(",", ".", temperaturaConv$Max))
  temperaturaConv$Min <- as.numeric(gsub(",", ".", temperaturaConv$Min))
  
  # Opcional: para salvar o arquivo (descomente se necessário)
  # write_xlsx(temperaturaConv, path = "TEMPERATURA/dados_vakudar.xlsx")
  
  return(temperaturaConv)
}
