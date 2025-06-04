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

baseRegressao <- baseRegressao[!baseRegressao$causabas_capitulo %in% c('X. Doenças do aparelho respiratório', 'II. Neoplasias (tumores)'), ]

#baseRegressao <- baseRegressao[!baseRegressao$ESTADO %in% c('AL', 'BA', 'CE', 'MA', 'PB', 'PE', 'PI', 'RN', 'SE', 'AC', 'AP', 'AM', 'PA', 'RO', 'RR', 'TO'), ]

#codigos_ibge_capitais <- c(110020, 120040, 130260, 140010, 150140, 160030, 172100, 211130, 221100, 230440, 240810, 250750, 261160, 270430, 280030, 292740, 310620, 320530, 330455, 355030, 410690, 420540, 431490, 500270, 510340, 520870, 530010)
#baseRegressao <- baseRegressao[baseRegressao$CODMUNRES %in% codigos_ibge_capitais, ]

ibge <- read_csv("TEMPERATURA/MUNICIPIOS/ibge_2002_2019_comPopulacao.csv")

ibge <- ibge %>%
  filter(POPULACAO > 0 & PIB > 0) %>%
  mutate(IBGE = floor(IBGE / 10))

tudo =
  baseRegressao %>%
  group_by(ano_obito, DTOBITO, CODMUNRES, def_sexo, def_raca_cor, idoso, temperatura_maxima) %>%
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
  mutate(taxa_mortalidade = 100 * number_deaths / POPULACAO)

toRegress <- toRegress %>%  mutate(Y25 = ifelse(temperatura_maxima >= 25, TRUE, FALSE))
toRegress <- toRegress %>%  mutate(Y30 = ifelse(temperatura_maxima >= 30, TRUE, FALSE))
toRegress <- toRegress %>%  mutate(Y35 = ifelse(temperatura_maxima >= 35, TRUE, FALSE))
toRegress <- toRegress %>%  mutate(Y40 = ifelse(temperatura_maxima >= 40, TRUE, FALSE))


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

resultados_modelos <- list()

modelo <- feols(data = toRegress,
                  fml = taxa_mortalidade ~  Y25 | DTOBITO, cluster = ~ DTOBITO)
  
model_name <- paste0('Temp. acima 25 C°')
resultados_modelos[[model_name]] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  Y30 | DTOBITO, cluster = ~ DTOBITO)

model_name <- paste0('Temp. acima 30 C°')
resultados_modelos[[model_name]] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  Y35 | DTOBITO, cluster = ~ DTOBITO)

model_name <- paste0('Temp. acima 35 C°')
resultados_modelos[[model_name]] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  is_white + is_male + idoso  + Y40 | DTOBITO)

model_name <- paste0('Temp. acima 40 C°')
resultados_modelos[[model_name]] <- modelo

etable(resultados_modelos)
