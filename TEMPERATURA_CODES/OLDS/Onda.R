library(dplyr)
library(lubridate)

rle_anomalia <- rle(temperatura$anomalia_max)

# Cria vetor onda_max preenchido com 0
temperatura$onda_max <- 0

# Índices cumulativos dos valores
ends <- cumsum(rle_anomalia$lengths)
starts <- ends - rle_anomalia$lengths + 1

# Aplica 1 na nova coluna onde anomalia == 1 e comprimento >= 5
for (i in seq_along(rle_anomalia$values)) {
  if (rle_anomalia$values[i] == 1 && rle_anomalia$lengths[i] >= 5) {
    temperatura$onda_max[starts[i]:ends[i]] <- 1
  }
}
    

rle_anomalia <- rle(temperatura$anomalia_min)

# Cria vetor onda_max preenchido com 0
temperatura$onda_min <- 0

# Índices cumulativos dos valores
ends <- cumsum(rle_anomalia$lengths)
starts <- ends - rle_anomalia$lengths + 1

# Aplica 1 na nova coluna onde anomalia == 1 e comprimento >= 5
for (i in seq_along(rle_anomalia$values)) {
  if (rle_anomalia$values[i] == 1 && rle_anomalia$lengths[i] >= 5) {
    temperatura$onda_min[starts[i]:ends[i]] <- 1
  }
}

#nome_arquivo_saida <- paste0('TEMPERATURA/DADOS/BASE_FINAL_TEMPERATURA', '.csv')
#write.csv(dados_final, file = nome_arquivo_saida, row.names = FALSE, quote = TRUE, na = "")