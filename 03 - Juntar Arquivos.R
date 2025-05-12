library(dplyr)

# Função para gerar a sequência de anos
lista_range_ano <- function(inicio, fim) {
  return(inicio:fim)
}

# Define o intervalo de anos
anos <- lista_range_ano(2019, 2019)

# Loop através dos anos
for (ano in anos) {
  # 1. Especifique o caminho da pasta usando o ano atual
  pasta_ano <- paste0("C:/Users/cristiano.silva/Documents/PROJETO/DADOS/", ano, "/") # Adicionei uma barra no final para garantir que é um diretório
  
  # 2. Liste todos os arquivos CSV na pasta do ano
  arquivos_csv <- list.files(pasta_ano, pattern = "\\.csv$", full.names = TRUE) # Corrigido: list.files precisa do caminho da pasta do ano e o padrão ".csv" para pegar só csv e full.names = TRUE
  
  # 3. Crie uma lista vazia para armazenar os dados
  lista_de_dados <- list()
  
  # 4. Loop através dos arquivos, leia cada um e adicione à lista
  if (length(arquivos_csv) > 0) { # Adiciona verificação se há arquivos para processar
    for (arquivo_csv in arquivos_csv) { # Renomeei a variável do loop para evitar confusão
      dados <- read.csv(arquivo_csv, header = TRUE, sep = ",", encoding = "UTF-8")
      lista_de_dados[[arquivo_csv]] <- dados
    }
    
    # 5. Combine todos os data frames em um único, se houver arquivos
    dados_combinados <- do.call(rbind, lista_de_dados)
    
    # 6. Especifique o nome do arquivo de saída usando o ano atual
    nome_arquivo_saida <- paste0("C:/Users/cristiano.silva/Documents/PROJETO/DADOS/COMBINADOS_", ano, ".csv") # Nome de arquivo diferente para cada ano
    
    # 7. Escreva o data frame para um arquivo CSV
    write.csv(dados_combinados, file = nome_arquivo_saida, row.names = FALSE, quote = TRUE, na = "")
    cat(paste("Arquivos do ano", ano, "combinados e salvos em", nome_arquivo_saida, "\n")) # feedback
  } else {
    cat(paste("Nenhum arquivo CSV encontrado para o ano", ano, "na pasta", pasta_ano, "\n"))
  }
}
