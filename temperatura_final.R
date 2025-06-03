library(dplyr)
library(lubridate) # Embora não explicitamente usado neste bloco, é bom manter se for parte do fluxo completo.

#' @title Calcula anomalias de temperatura a partir de dados processados
#' @description Agrupa os dados de temperatura por município e data, calcula as médias
#'   e as anomalias de temperatura máxima e mínima em relação às normais climatológicas.
#' @param temperaturaAuto Um dataframe contendo os dados de temperatura automática processados,
#'   incluindo colunas como `SG_ESTADO`, `Codigo_IBGE`, `DATA`, `Mes`,
#'   `temperatura_maxima`, `temperatura_minima`, `Max` (normal), e `Min` (normal).
#' @param temperaturaConv Opcional. Um dataframe contendo os dados de temperatura convencional processados,
#'   com a mesma estrutura de `temperaturaAuto`. (Atualmente comentado no corpo da função)
#' @return Um dataframe `temperatura` com as médias diárias de temperatura,
#'   as diferenças em relação às normais e indicadores de anomalia.
#' @examples
#' # Supondo que 'temperatura_processada_2019' foi gerado pela função anterior:
#' # resultado_final <- calcula_anomalias_temperatura(temperatura_processada_2019)
calcula_anomalias_temperatura <- function(temperaturaAuto, temperaturaConv) {
  
  temperatura <- rbind(temperaturaAuto, temperaturaConv)
  
  temperatura<- temperatura %>%
    filter(!is.na(Codigo_IBGE))
  
  # Agrupa por estado, código IBGE, data e mês para calcular as médias
  temperatura <- temperatura %>%
    group_by(ESTADO, Codigo_IBGE, DATA, Mes) %>%
    summarise(
      media_temperatura_maxima = mean(temperatura_maxima, na.rm = TRUE),
      media_temperatura_minima = mean(temperatura_minima, na.rm = TRUE),
      media_max = mean(Max, na.rm = TRUE),
      media_min = mean(Min, na.rm = TRUE),
      .groups = 'drop' # Adiciona .groups = 'drop' para remover o agrupamento após summarise
    )
  
  # Calcula as diferenças em relação às normais
  temperatura$diff_max <- temperatura$media_temperatura_maxima - temperatura$media_max
  temperatura$diff_min <- temperatura$media_temperatura_minima - temperatura$media_min
  
  # Cria indicadores de anomalia
  temperatura <- temperatura %>%
    mutate(anomalia_max = ifelse(diff_max >= 5, 1, 0)) %>%
    mutate(anomalia_min = ifelse(diff_min <= -5, 1, 0))
  
  # Filtra linhas onde as médias das normais (Max ou Min) não são NA
  temperatura <- temperatura %>%
    filter(!is.na(media_max) | !is.na(media_min))
  
  return(temperatura)
}