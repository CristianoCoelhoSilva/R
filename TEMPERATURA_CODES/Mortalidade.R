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

codigos_ibge_capitais <- c(110020, 120040, 130260, 140010, 150140, 160030, 172100, 211130, 221100, 230440, 240810, 250750, 261160, 270430, 280030, 292740, 310620, 320530, 330455, 355030, 410690, 420540, 431490, 500270, 510340, 520870, 530010)
mortalidade <- mortalidade[mortalidade$CODMUNRES %in% codigos_ibge_capitais, ]

indices_7_digitos <- nchar(as.character(mortalidade$DTOBITO)) == 7
mortalidade$DTOBITO[indices_7_digitos] <- paste0("0", mortalidade$DTOBITO[indices_7_digitos])
mortalidade$DTOBITO <- as.Date(as.character(mortalidade$DTOBITO), format = "%d%m%Y")

base <- mortalidade %>%
  inner_join(temperatura, by = c("CODMUNRES" = "Codigo_IBGE", "DTOBITO" = "DATA"), relationship = "many-to-many")
