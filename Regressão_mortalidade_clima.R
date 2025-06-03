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
#base <- dados_combinados[dados_combinados$causabas_capitulo == 'IX.  Doenças do aparelho circulatório',]

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

toRegressFiltro <- toRegress[toRegress$temperatura_maxima <= 44.9, ]

modelo <- feols(data = toRegressFiltro
                , fml = taxa_mortalidade ~  temp_44 | DTOBITO)

resultados_modelos[[doenca_desc]] <- modelo

etable(modelo)