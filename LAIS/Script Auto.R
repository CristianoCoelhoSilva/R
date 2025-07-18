  library(dplyr)
  library(tidyr)
  library(readxl)
  
  #data <- read_excel("Excel/Combatendo as Mudanças Climáticas.xlsx")
  #data <- data[-1, ]
  
  data <- Grupo1
  data <- data[-1, ]
  
  data <- data[c(11,12,13,14,15,16,17)]
  
  column_names_to_process <- names(data)[1:7]
  
  lista_strings <- c()
  
  for (col_name in column_names_to_process) {
    
    if (col_name %in% names(data)) {
      
      data[[col_name]] <- as.factor(data[[col_name]])
      categorias <- levels(data[[col_name]])
      n_questao <- which(names(data) == col_name)
      
      for (i in seq_along(categorias)) {  # i vai de 1 até número de categorias
        categoria <- categorias[i]
        
        nome_dummy <- paste0(
          "VF", n_questao, "_", i,
          " = ifelse(`", col_name, "` == '", categoria, "', 1, 0)"
        )
        
        lista_strings <- c(lista_strings, nome_dummy)
      }
      
    } else {
      warning(paste("Column", col_name, "not found in the dataframe."))
    }
  }
  
  print(lista_strings)
  
  #df_strings <- data.frame(expressao = lista_strings, stringsAsFactors = FALSE)
  
  #write.csv(df_strings, "Excel/expressao_dummies_numero.csv", row.names = FALSE)
  