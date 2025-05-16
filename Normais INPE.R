normais_estacoes <- function() {
library(readxl)
library(dplyr)
library(tidyr)

normais_maxima <- read_excel("TEMPERATURA/DADOS/Normal-Climatologica-TMAX.xlsx")
normais_maxima <- normais_maxima[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)]

# Primeiro, transforme os dados de formato largo para longo
normais_maxima <- normais_maxima %>%
  pivot_longer(
    cols = Janeiro:Dezembro,
    names_to = "Mes",
    values_to = "Temp_maxima"
  )


normais_minima  <- read_excel("TEMPERATURA/DADOS/Normal-Climatologica-TMIN.xlsx")
normais_minima <- normais_minima[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)]

# Primeiro, transforme os dados de formato largo para longo
normais_minima <- normais_minima %>%
  pivot_longer(
    cols = Janeiro:Dezembro,
    names_to = "Mes",
    values_to = "Temp_minima"
  )

normais <- normais_maxima %>%
  inner_join(
    normais_minima,
    by = c("Nome da Estação" = "Nome da Estação", "UF" = "UF", "Mes" = "Mes"),
    relationship = "many-to-many"
  )

estacoes <- read_excel("TEMPERATURA/DADOS/estacoes.xlsx")

estacoes <- estacoes %>%
  mutate(estacao = toupper(estacao))

dados <- estacoes %>%
  inner_join(
    normais,
    by = c("estacao" = "Nome da Estação"),
    relationship = "many-to-many"
  )
return(dados)

}