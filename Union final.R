library(readr)
library(dplyr)
library(purrr) # Para a função map_df

pasta_de_arquivos <- 'TEMPERATURA/TEMPERATURA/TODAS_AUTO'

# --- 3. Listar todos os arquivos na pasta ---
# Use o padrão "*.csv" se você tiver outros tipos de arquivos na pasta e quiser apenas CSVs.
# full.names = TRUE garante que o caminho completo do arquivo seja retornado.
todos_os_arquivos <- list.files(path = pasta_de_arquivos, pattern = "\\.csv$", full.names = TRUE)

cat("Arquivos encontrados:\n")
print(todos_os_arquivos)
cat("\n")

# --- 4. Ler e combinar todos os arquivos em um único dataframe ---
# A função map_df() do pacote purrr é excelente para isso:
# Ela aplica uma função (read_csv neste caso) a cada elemento de uma lista (todos_os_arquivos)
# e combina os resultados em um único dataframe (df).
dados_combinados <- map_df(todos_os_arquivos, read_csv)

cat("DataFrame combinado (primeiras 6 linhas):\n")
print(head(dados_combinados))