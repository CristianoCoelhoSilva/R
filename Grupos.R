library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(openxlsx)
data <- read_excel("NEWS PROJETOS/NEWS PROJETOS/DIOGO/Diogo Perfis de Racionalidade.xlsx", .name_repair = "minimal")

names(data)[names(data) == ""] <- paste0("unnamed_", seq_len(sum(names(data) == "")))


corrigir_cabecalho <- function(df) {

  colnames(df) <- as.character(unlist(df[1, ]))
  
  df <- df[-1, ]
  
  rownames(df) <- NULL
  
  return(df)}

data <- corrigir_cabecalho(data)