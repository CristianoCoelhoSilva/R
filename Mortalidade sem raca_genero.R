library(readr)
library(dplyr)
library(lubridate)
library(readxl) # Necessário para read_excel se a função anterior não for chamada
library(tidyr)  # Necessário para pivot_longer/wider se a função anterior não for chamada
library(writexl) # Necessário para write_xlsx
library(readr)
library(dplyr)
library(purrr) # Para a função map_df

pasta_de_arquivos <- 'MORTALIDADE/ESTUDO'

todos_os_arquivos <- list.files(path = pasta_de_arquivos, pattern = "\\.csv$", full.names = TRUE)

mortalidade <- map_df(todos_os_arquivos, read_csv)

mortalidade <- mortalidade[c(2,5,6,8,12,13,14)]

indices_7_digitos <- nchar(as.character(mortalidade$DTOBITO)) == 7
mortalidade$DTOBITO[indices_7_digitos] <- paste0("0", mortalidade$DTOBITO[indices_7_digitos])
mortalidade$DTOBITO <- as.Date(as.character(mortalidade$DTOBITO), format = "%d%m%Y")

base <- mortalidade %>%
  inner_join(temperatura, by = c("CODMUNRES" = "Codigo_IBGE", "DTOBITO" = "DATA"), relationship = "many-to-many")

rm(list = setdiff(ls(), "base"))