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

###filtrar para regressão do range de temperatura
baseRegressao <- baseRegressao[c("CODMUNRES"
                                , "def_sexo"
                                , "def_raca_cor"
                                , "causabas_capitulo"
                                , "idade_obito"
                                , "ano_obito"
                                , "DTOBITO"
                                , "Mes"
                                , "Ano"
                                , "DC_NOME"
                                , "ESTADO"
                                , "temperatura_maxima"
                                , "DIA_MES"
                                , "Y34"
                                , "Y36"
                                , "Y38"
                                , "Y40")]

baseRegressao <- baseRegressao %>%  mutate(Idoso = ifelse(idade_obito >= 60, TRUE, FALSE))

baseRegressao$idade_obito <- NULL

baseRegressao <- baseRegressao[!baseRegressao$causabas_capitulo %in% c('XX.  Causas externas de morbidade e mortalidade'
                                                                      ,'VII. Doenças do olho e anexos'
                                                                      ,'VIII.Doenças do ouvido e da apófise mastóide'
                                                                      ,'XV.  Gravidez parto e puerpério'), ]

ibge <- read_csv("TEMPERATURA/MUNICIPIOS/ibge_2002_2023_comPopulacao.csv")

tudo =
  baseRegressao %>%
  group_by(ano_obito, DTOBITO, CODMUNRES, def_sexo, def_raca_cor, Idoso, temperatura_maxima, DIA_MES, Y34, Y36, Y38, Y40) %>%
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
  filter(!(def_raca_cor %in% c("Ignorado", "Amarela","Indígena"))) %>%
  mutate(is_preta = def_raca_cor == "Preta")  %>%
  mutate(is_parda = def_raca_cor == "Parda")


toRegress <- toRegress %>%
  filter(!(def_sexo %in% c("Ignorado"))) %>%
  mutate(is_male = def_sexo == "Masculino")

resultados_modelos <- list()

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Y34 + Y36 + Y38 + Y40 | DTOBITO)

resultados_modelos[['I']] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Y34 + Y36 + Y38 + Y40  + is_male| DTOBITO)

resultados_modelos[['II']] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Y34 + Y36 + Y38 + Y40  + is_preta + is_parda| DTOBITO)

resultados_modelos[['III']] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Y34 + Y36 + Y38 + Y40 + Idoso| DTOBITO)

resultados_modelos[['IV']] <- modelo

etable(resultados_modelos, 
       dict = c(
         Y34TRUE = "Tempeture 34°C",
         Y36TRUE = "Tempeture 36°C",
         Y38TRUE = "Tempeture 38°C",
         Y40TRUE = "Tempeture 40°C",
         is_maleTRUE = "Man",
         is_pretaTRUE = "Black",
         is_pardaTRUE = "Brown",
         IdosoTRUE = "Old",
         DTOBITO = "Day Death"
       ))

resultados_modelos <- list()

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Y34 + Y36 + Y38 + Y40 | DIA_MES^CODMUNRES)

resultados_modelos[['I']] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Y34 + Y36 + Y38 + Y40  + is_male| DIA_MES^CODMUNRES)

resultados_modelos[['II']] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Y34 + Y36 + Y38 + Y40  + is_preta + is_parda| DIA_MES^CODMUNRES)

resultados_modelos[['III']] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Y34 + Y36 + Y38 + Y40 + Idoso| DIA_MES^CODMUNRES)

resultados_modelos[['IV']] <- modelo

etable(resultados_modelos, 
       dict = c(
         Y34TRUE = "Tempeture 34°C",
         Y36TRUE = "Tempeture 36°C",
         Y38TRUE = "Tempeture 38°C",
         Y40TRUE = "Tempeture 40°C",
         is_maleTRUE = "Man",
         is_pretaTRUE = "Black",
         is_pardaTRUE = "Brown",
         IdosoTRUE = "Old",
         DTOBITO = "Day Death"
       ))