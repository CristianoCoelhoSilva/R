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

baseRegressao <- baseRegressao %>%  mutate(Idoso = ifelse(idade_obito >= 60, TRUE, FALSE))

baseRegressao$idade_obito <- NULL

baseRegressao <- baseRegressao[!baseRegressao$causabas_capitulo %in% c('XX.  Causas externas de morbidade e mortalidade'), ]

ibge <- read_csv("TEMPERATURA/MUNICIPIOS/ibge_2002_2023_comPopulacao.csv")

tudo =
  baseRegressao %>%
  group_by(ano_obito, DTOBITO, CODMUNRES, def_sexo, def_raca_cor, Idoso, temperatura_maxima, temperatura_quadrado, Y34, Y36, Y38, Y40) %>%
  summarize(number_deaths = n()) %>%
  ungroup() %>%
  inner_join(ibge %>%
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

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ temperatura_maxima| DTOBITO)
etable(modelo)

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ temperatura_quadrado| DTOBITO)
etable(modelo)