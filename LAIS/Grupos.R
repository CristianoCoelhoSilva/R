library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(openxlsx)
data <- read_excel("Combatendo as Mudanças Climáticas.xlsx", .name_repair = "minimal")

Grupo1 <- data[data$Grupo == '1' | data$Grupo == 'Grupo',]
Grupo1 <- Grupo1 %>%
  select(13:20, 21:89, 293:300)

Grupo2 <- data[data$Grupo == '2' | data$Grupo == 'Grupo',]
Grupo2 <- Grupo2 %>%
  select(13:20, 90:158, 293:300)

Grupo3 <- data[data$Grupo == '3' | data$Grupo == 'Grupo',]
Grupo3 <- Grupo3 %>%
  select(13:20, 226:292, 293:300)

Grupo4 <- data[data$Grupo == '4' | data$Grupo == 'Grupo',]
Grupo4 <- Grupo4 %>%
  select(13:20, 159:225, 293:300)


corrigir_cabecalho <- function(df) {

  df[] <- lapply(df, as.character)
  
  for (j in 2:ncol(df)) {
    if (df[1, j] == "" || is.na(df[1, j])) {
      df[1, j] <- df[1, j - 1]
    }
  }
  
  colnames(df) <- as.character(unlist(df[1, ]))
  
  df <- df[-1, ]
  
  rownames(df) <- NULL
  
  return(df)
}

Grupo1 <- corrigir_cabecalho(Grupo1)
Grupo2 <- corrigir_cabecalho(Grupo2)
Grupo3 <- corrigir_cabecalho(Grupo3)
Grupo4 <- corrigir_cabecalho(Grupo4)

write.xlsx(Grupo1, file = "Grupo1.xlsx")
write.xlsx(Grupo2, file = "Grupo2.xlsx")
write.xlsx(Grupo3, file = "Grupo3.xlsx")
write.xlsx(Grupo4, file = "Grupo4.xlsx")