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
                                 , "Extrema"
                                 , "Severa"
                                 , "Baixa")]

baseRegressao <- baseRegressao %>%  mutate(Idoso = ifelse(idade_obito >= 60, TRUE, FALSE))

baseRegressao$idade_obito <- NULL

baseRegressao <- baseRegressao[!baseRegressao$causabas_capitulo %in% c('XX.  Causas externas de morbidade e mortalidade'
                                                                       ,'VII. Doenças do olho e anexos'
                                                                       ,'VIII.Doenças do ouvido e da apófise mastóide'
                                                                       ,'XV.  Gravidez parto e puerpério'), ]

ibge <- read_csv("TEMPERATURA/MUNICIPIOS/ibge_2002_2023_comPopulacao.csv")

tudo =
  baseRegressao %>%
  group_by(ano_obito, DTOBITO, CODMUNRES, def_sexo, def_raca_cor, Idoso, temperatura_maxima, DIA_MES, Extrema, Severa, Baixa) %>%
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
                fml = taxa_mortalidade ~ Baixa + Severa + Extrema| DIA_MES^CODMUNRES)

resultados_modelos[['I']] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Baixa + Severa + Extrema + is_male| DIA_MES^CODMUNRES)

resultados_modelos[['II']] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Baixa + Severa + Extrema + is_preta + is_parda| DIA_MES^CODMUNRES)

resultados_modelos[['III']] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~ Baixa + Severa + Extrema + Idoso| DIA_MES^CODMUNRES)

resultados_modelos[['IV']] <- modelo

etable(resultados_modelos, 
       dict = c(
         BaixaTRUE = "Low-intensity",
         SeveraTRUE = "Severe",
         ExtremaTRUE = "Extreme",
         is_maleTRUE = "Man",
         is_pretaTRUE = "Black",
         is_pardaTRUE = "Brown",
         IdosoTRUE = "Old",
         DTOBITO = "Day Death"
       ))
