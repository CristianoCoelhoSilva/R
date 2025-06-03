library(readr)
library(dplyr)
library(lubridate)
library(readxl) # Necessário para read_excel se a função anterior não for chamada
library(tidyr)  # Necessário para pivot_longer/wider se a função anterior não for chamada
library(writexl) # Necessário para write_xlsx
library(readr)
library(dplyr)
library(purrr) # Para a função map_df

pasta_de_arquivos <- 'TEMPERATURA/MORTALIDADE/ESTUDO'

todos_os_arquivos <- list.files(path = pasta_de_arquivos, pattern = "\\.csv$", full.names = TRUE)

mortalidade <- map_df(todos_os_arquivos, read_csv)

mortalidade <- mortalidade[c(2,5,6,8,12,13,14)]

indices_7_digitos <- nchar(as.character(mortalidade$DTOBITO)) == 7
mortalidade$DTOBITO[indices_7_digitos] <- paste0("0", mortalidade$DTOBITO[indices_7_digitos])
mortalidade$DTOBITO <- as.Date(as.character(mortalidade$DTOBITO), format = "%d%m%Y")

mortalidade = mortalidade %>%
  group_by(ano_obito, DTOBITO, idade_obito, def_sexo, def_raca_cor, CODMUNRES, causabas_capitulo) %>%
  summarize(number_deaths = n(), .groups = "drop")

temperatura$Codigo_IBGE <- substr(temperatura$Codigo_IBGE, 1, 6)
temperatura$Codigo_IBGE <- as.numeric(temperatura$Codigo_IBGE)

base <- mortalidade %>%
  inner_join(temperatura, by = c("CODMUNRES" = "Codigo_IBGE", "DTOBITO" = "DATA"))

base <- base[c(1,2,3,4,5,6,7,8,14,15,16,17,18,19,20,21,22,23)]

rm(list = setdiff(ls(), "base"))


gc()
