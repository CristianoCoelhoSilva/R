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

baseRegressao <- baseRegressao %>%  mutate(idoso = ifelse(idade_obito >= 60, TRUE, FALSE))

baseRegressao$idade_obito <- NULL

baseRegressao <- baseRegressao[!baseRegressao$causabas_capitulo %in% c('X. Doenças do aparelho respiratório'), ]

ibge <- read_csv("TEMPERATURA/MUNICIPIOS/ibge_2002_2019_comPopulacao.csv")

ibge <- ibge %>%
  filter(POPULACAO > 0 & PIB > 0) %>%
  mutate(IBGE = floor(IBGE / 10))

tudo =
  baseRegressao %>%
  group_by(ano_obito, DTOBITO, CODMUNRES, def_sexo, def_raca_cor, idoso, onda_calor) %>%
  summarize(number_deaths = n()) %>%
  ungroup() %>%
  left_join(ibge %>%
              select(ano_obito = ANO,
                     CODMUNRES = IBGE,
                     POPULACAO,
                     REGIAO
              ))

toRegress =
  tudo %>%
  mutate(taxa_mortalidade = 100000 * number_deaths / POPULACAO)

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

#resultados_modelos <- list()

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  onda_calor | DTOBITO)

etable(modelo)
