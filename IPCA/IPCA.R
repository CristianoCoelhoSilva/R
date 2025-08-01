# Carrega as bibliotecas necessárias para manipulação de dados
library(readxl)
library(stringr)
library(dplyr) # O pacote dplyr é útil para manipulação de data frames
library(purrr) # O pacote purrr é ideal para iterar sobre listas de arquivos

# 1. Define o caminho para a pasta que contém os arquivos
data_folder <- "NEWS PROJETOS/POF/dados_pof/"

# 2. Lista todos os arquivos que correspondem ao padrão de nome
# O padrão 'ipca_\\d{6}Subitem.xls' irá encontrar arquivos como 'ipca_201902Subitem.xls'
# O '\\d{6}' procura exatamente 6 dígitos numéricos.
file_list <- list.files(path = data_folder,
                        pattern = "ipca_\\d{6}Subitem\\.xls",
                        full.names = TRUE)

# Verifica se os arquivos foram encontrados
if (length(file_list) == 0) {
  stop("Nenhum arquivo encontrado. Verifique o caminho e o padrão do nome do arquivo.")
}

# 3. Itera sobre a lista de arquivos, processa cada um e combina os resultados
# map_dfr aplica uma função a cada elemento de uma lista e combina os resultados por linha
# A função anônima (.x) processa cada arquivo individualmente
final_ipca_data <- map_dfr(file_list, function(.x) {
  
  # a. Extrai a parte da data do nome do arquivo (ex: "201902")
  file_name <- basename(.x)
  date_part <- str_extract(file_name, "\\d{6}")
  
  # b. Formata a data para "mm/aaaa" (ex: "02/2019")
  formatted_date <- paste0(substr(date_part, 5, 6), "/", substr(date_part, 1, 4))
  
  # c. Lê o arquivo Excel, ignorando a primeira linha que não é cabeçalho
  ipca_temp <- read_excel(.x, sheet = "MENSAL SUBITEM IPCA")
  
  # d. Filtra as linhas 4 e 6 (considerando o arquivo original)
  ipca_temp <- ipca_temp[c(4, 6), ]
  
  # e. Usa a primeira linha filtrada como novo cabeçalho e remove-a
  header_row <- as.character(ipca_temp[1, ])
  colnames(ipca_temp) <- header_row
  ipca_temp <- ipca_temp[-1, ]
  
  # f. Garante que as colunas numéricas sejam arredondadas para 2 casas
  ipca_temp <- as.data.frame(lapply(ipca_temp, function(col) {
    col_numeric <- suppressWarnings(as.numeric(as.character(col)))
    if (is.numeric(col_numeric)) {
      return(round(col_numeric, 2))
    } else {
      return(col)
    }
  }))
  
  ipca_temp$`Mes_ano` <- formatted_date
  
  ipca_temp$NA. <- NULL
  
  # i. Reordena as colunas para que 'Mes_ano' seja a primeira
  ipca_temp <- ipca_temp[, c("Mes_ano", setdiff(names(ipca_temp), "Mes_ano"))]
  
  # Retorna o data frame processado para ser combinado
  return(ipca_temp)
})

# Exibe o resultado final combinado
print(final_ipca_data)