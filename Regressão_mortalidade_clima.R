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
rm(list = setdiff(ls(), "base"))

baseRegressao <- base

baseRegressao <- baseRegressao[!baseRegressao$causabas_capitulo == 'XX.  Causas externas de morbidade e mortalidadea',]

#baseRegressao <- baseRegressao[!baseRegressao$ESTADO %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'RN', 'SE', 'AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO'), ]

#codigos_ibge_capitais <- c(110020, 120040, 130260, 140010, 150140, 160030, 172100, 211130, 221100, 230440, 240810, 250750, 261160, 270430, 280030, 292740, 310620, 320530, 330455, 355030, 410690, 420540, 431490, 500270, 510340, 520870, 530010)
#baseRegressao <- baseRegressao[baseRegressao$CODMUNRES %in% codigos_ibge_capitais, ]

ibge <- read_csv("TEMPERATURA/MUNICIPIOS/ibge_2002_2019_comPopulacao.csv")

ibge <- ibge %>%
    filter(POPULACAO > 0 & PIB > 0) %>%
    mutate(IBGE = floor(IBGE / 10))

toRegress =
  baseRegressao %>%
  inner_join(ibge %>%
              select(ano_obito = ANO,
                     CODMUNRES = IBGE,
                     POPULACAO,
                     REGIAO
              ))

toRegress$Jovem <- ifelse(toRegress$idade_obito <= 19, 1, 0)
toRegress$Idoso <- ifelse(toRegress$idade_obito >= 60, 1, 0)

toRegress <- toRegress %>%
    filter(!(def_raca_cor %in% c("Ignorado", "Amarela"))) %>%
    mutate(def_raca_cor = case_when(
      def_raca_cor %in% c("Branca") ~ "Branca",
      TRUE ~ "Não Branca"
    )) %>%
    mutate(is_white = def_raca_cor == "Branca")

toRegress <- toRegress %>%
    filter(!(def_sexo %in% c("Ignorado"))) %>%
    mutate(def_sexo = case_when(
      def_sexo %in% c("Masculino") ~ "Masculino",
      TRUE ~ "Feminino"
    )) %>%
    mutate(is_male = def_sexo == "Masculino")
  

#names(toRegress) <- c("ano_obito", "data", "clima", "Age", "Gender", "Race", "Municipio", "number_deaths", "POPULACAO", "REGIAO", "Jovem","Idoso","Is_white","Is_male")

toRegress$taxa_mortalidade <- ((100 *  toRegress$number_deaths) / toRegress$POPULACAO)

resultados_modelos <- list()

# Loop do 37 ao 45
for (i in 37:44) {


  explanatory_var_name <- paste0("temp_", i)
  
  fml_formula <- as.formula(paste0("taxa_mortalidade ~ ", explanatory_var_name, " | DTOBITO"))
  
  print(fml_formula)
  
  modelo <- feols(data = toRegress,
                   fml = fml_formula, cluster = ~ DTOBITO + causabas_capitulo)
    
  model_name <- paste0('Temperatura ', i, '°C ')
  resultados_modelos[[model_name]] <- modelo
    
}

etable(resultados_modelos)
