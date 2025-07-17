library(dplyr)
library(tidyr)
library(readxl)

data <- Grupo1
#data <- data[-1, ]

#data <- data[c(31,32,33,34,76,77)]

colnames(data) <- make.unique(colnames(data))

#Questão 18
data <- data %>% select(18:30)

data <- corrigir_cabecalho(data)

column_names_to_process <- names(data)[1:13]

lista_strings <- c()

for (col_name in column_names_to_process) {
  
  if (col_name %in% names(data)) {
    
    data[[col_name]] <- as.factor(data[[col_name]])
    categorias <- levels(data[[col_name]])
    n_questao <- which(names(data) == col_name)
    
    for (i in seq_along(categorias)) {  # i vai de 1 até número de categorias
      categoria <- categorias[i]
      
      nome_dummy <- paste0("`",col_name, "` == '", categoria, "' ~ ",i,",remove"
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
