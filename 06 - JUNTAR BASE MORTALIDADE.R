library(dplyr)

  pasta_ano <- paste0('TEMPERATURA/DADOS/MORTALIDADE/2008_2019')

  arquivos_csv <- list.files(pasta_ano, pattern = "\\.csv$", full.names = TRUE) 
  
  
  lista_de_dados <- list()
  

  if (length(arquivos_csv) > 0) {
    for (arquivo_csv in arquivos_csv) {
      dados <- read.csv(arquivo_csv, header = TRUE, sep = ",", encoding = "UTF-8")
      
      colnames(dados) <- tolower(colnames(dados))
      dados <- dados[, c("codmunres", "codmunnatu", "idade", "def_sexo", "def_raca_cor", "def_escol", "causabas_capitulo", "causabas_categoria", "causabas_grupo", "causabas_subcategoria", "idade_obito", "ano_obito", "dtobito")]
      
      
      
      lista_de_dados[[arquivo_csv]] <- dados
      
      print(arquivo_csv)
    }
    
    dados <- do.call(rbind, lista_de_dados)
    
  }
  
  nome_arquivo_saida <- paste0('TEMPERATURA/DADOS/MORTALIDADE/MORTALIDADE_2008_2019', '.csv')
  write.csv(dados, file = nome_arquivo_saida, row.names = FALSE, quote = TRUE, na = "")
  