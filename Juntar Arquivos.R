library(readr)
library(dplyr)

# Especifica o diretório onde os arquivos CSV estão localizados
diretorio <- "~/TEMPERATURA/2024/TRATADOS/" # Substitua pelo seu diretório real

# Lista todos os arquivos CSV no diretório
arquivos_csv <- list.files(diretorio, pattern = "\\.CSV$", full.names = TRUE)

# Inicializa um dataframe vazio para armazenar os dados combinados
dados_combinados <- data.frame()

# Loop através de cada arquivo CSV
for (arquivo in arquivos_csv) {
  # Lê o arquivo CSV
  dados_arquivo <- read_csv(arquivo)
  
  # Extrai o nome do arquivo (sem o caminho)
  nome_arquivo <- basename(arquivo)
  
  # Adiciona uma coluna com o nome do arquivo
  dados_arquivo <- dados_arquivo %>%
    mutate(nome_do_arquivo = nome_arquivo)
  
  # Combina os dados do arquivo atual com o dataframe combinado
  dados_combinados <- bind_rows(dados_combinados, dados_arquivo)
}

dados_agrupados <- dados_combinados %>%
  group_by(nome_do_arquivo)

# Conta o número de linhas em cada grupo (ou seja, a quantidade de registros por arquivo)
qtd_por_arquivo <- dados_agrupados %>%
  summarise(quantidade = n())
