  #rm(list = ls())
  options(scipen = 999)
  library(data.table)
  library(bit64)
  library(tidyverse)
  library(stargazer)
  library(lfe)
  library(lubridate)
  library(gridExtra)
  library(grid)
  library(fixest)
  library(sqldf)
  library(readr)
  library(dplyr)
  set.seed('123')
  library(readxl)
  
  
  toRegress <- read_excel("TEMPERATURA/toRegress.xlsx")
  
  #toRegress <- toRegress[toRegress$DTOBITO == as.Date('2023-01-01'),]
               
  #toRegress <- toRegress[c(1,2,3,7,8,9)]
  
  toRegress <- toRegress %>%
    group_by(ano, DTOBITO, anomalia_max, CODMUNRES) %>%
    summarise(
      number_deaths = sum(number_deaths),
      POPULACAO = mean(POPULACAO)
    ) %>%
    ungroup()
  
  toRegress =
    toRegress %>%
    mutate(taxa_mortalidade = number_deaths / POPULACAO)
  
  
  toRegress$log_taxa_mortalidade <- ((100 *  toRegress$number_deaths) / toRegress$POPULACAO)
  
  
  modelo <- feols(data = toRegress
                  ,fml = log_taxa_mortalidade ~  anomalia_max | DTOBITO
  )
  
  etable(modelo)
