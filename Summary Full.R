calcular_estatisticas <- function(data) {
 
  # Função para calcular estatísticas
  calcular_estatisticas <- function(coluna) {
    media <- mean(coluna, na.rm = TRUE)
    mediana <- median(coluna, na.rm = TRUE)
    desvio_padrao <- sd(coluna, na.rm = TRUE)
    variancia <- var(coluna, na.rm = TRUE)
    assimetria <- skewness(coluna, na.rm = TRUE)
    curtose <- kurtosis(coluna, na.rm = TRUE)
    minimo <- min(coluna, na.rm = TRUE)
    maximo <- max(coluna, na.rm = TRUE)
    jarque_bera <- jarque.bera.test(coluna)
    p_valor_jb <- jarque_bera$statistic
    
    c(media, mediana, desvio_padrao, variancia, assimetria, curtose, minimo, maximo, p_valor_jb)
  }
  
  # Aplicar a função para cada coluna
  colunas_desejadas <- names(data)
  lista_resultados <- lapply(data[colunas_desejadas], calcular_estatisticas)
  
  # Criar o data frame final
  tabela_final <- data.frame(
    Estatistica = c("Media", "Median", "Std. Deviation", "Variance", "Skewness", "kurtosis", "Min", "Max", "Jarque-Bera")
  )
  
  # Adicionar os valores das colunas
  for (i in seq_along(colunas_desejadas)) {
    tabela_final[[colunas_desejadas[i]]] <- lista_resultados[[i]]
  }
  
  # Retornar a tabela
  return(tabela_final)
}
