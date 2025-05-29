library(readr)

death <- read_csv("TEMPERATURA/MORTALIDADE/MORTALIDADE/FINAL2023.csv")

indices_7_digitos <- nchar(as.character(death$DTOBITO)) == 7
death$DTOBITO[indices_7_digitos] <- paste0("0", death$DTOBITO[indices_7_digitos])
death$DTOBITO <- as.Date(as.character(death$DTOBITO), format = "%d%m%Y")

temperatura$Codigo_IBGE <- substr(temperatura$Codigo_IBGE,1,6)

base <- death %>%
  inner_join(temperatura %>% mutate(Codigo_IBGE = as.double(Codigo_IBGE)),
             by = c("CODMUNRES" = "Codigo_IBGE", "DTOBITO" = "DATA"))


rm(list = setdiff(ls(), c("base")))