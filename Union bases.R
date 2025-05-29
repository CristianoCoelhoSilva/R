library(readr)

death <- read_csv("TEMPERATURA/MORTALIDADE/MORTALIDADE/FINAL2023.csv")

temperatura$Codigo_IBGE <- substr(temperatura$Codigo_IBGE,1,6)

base <- death %>%
  left_join(temperatura_municipio %>% mutate(Codigo_IBGE = as.double(Codigo_IBGE)),
             by = c("CODMUNRES" = "Codigo_IBGE"))