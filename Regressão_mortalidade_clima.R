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

ano_filtro <- 2019

#base <- death[death$causabas_capitulo == 'IX.  Doenças do aparelho circulatório',]

base <- death

base <- base %>%
  mutate(
    ano = year(DTOBITO) 
  )

ibge <- read_csv("TEMPERATURA/MORTALIDADE/ibge_2002_2019_comPopulacao.csv")
ibge <- ibge %>%
  filter(ANO == ano_filtro) %>% # Filtra somente as linhas onde a coluna 'Ano' é 2019
    filter(POPULACAO > 0 & PIB > 0) %>%
    mutate(IBGE = floor(IBGE / 10))
  
  tudo =
    base %>%
    mutate(discrete_idade_obito = dplyr::ntile(idade_obito, 10)) %>%
    group_by(ano, DTOBITO, anomalia_max
             , discrete_idade_obito, def_sexo, def_raca_cor
             , CODMUNRES) %>%
    summarize(number_deaths = n()) %>%
    ungroup() %>%
    left_join(ibge %>%
                select(ano = ANO,
                       CODMUNRES = IBGE,
                       POPULACAO,
                       REGIAO
                ))

toRegress <- tudo %>%
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
  
  names(toRegress) <- c("ano_obito", "data", "clima", "Age", "Gender", "Race", "Municipio", "number_deaths", "POPULACAO", "REGIAO", "is_white","is_male")
  
  toRegress$log_taxa_mortalidade <- ((100 *  toRegress$number_deaths) / toRegress$POPULACAO)
  toRegress$taxa_mortalidade <- (toRegress$number_deaths / toRegress$POPULACAO)
  
  modelo <- feols(data = toRegress
                , fml = log_taxa_mortalidade ~  clima | data)

etable(modelo)