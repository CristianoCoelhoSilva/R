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

#baseRegressao <- baseRegressao %>%  mutate(Idoso = ifelse(idade_obito >= 60, TRUE, FALSE))

baseRegressao <- baseRegressao %>%  mutate(idoso = ifelse(idade_obito >= 60, TRUE, FALSE))

baseRegressao$idade_obito <- NULL

baseRegressao <- baseRegressao[!baseRegressao$causabas_capitulo == 'XX.  Causas externas de morbidade e mortalidade',]

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


toRegress <- toRegress %>%  mutate(temp_37 = ifelse(temperatura_maxima >= 37, TRUE, FALSE))
toRegress <- toRegress %>%  mutate(temp_38 = ifelse(temperatura_maxima >= 38, TRUE, FALSE))
toRegress <- toRegress %>%  mutate(temp_39 = ifelse(temperatura_maxima >= 39, TRUE, FALSE))
toRegress <- toRegress %>%  mutate(temp_40 = ifelse(temperatura_maxima >= 40, TRUE, FALSE))
toRegress <- toRegress %>%  mutate(temp_41 = ifelse(temperatura_maxima >= 41, TRUE, FALSE))
toRegress <- toRegress %>%  mutate(temp_42 = ifelse(temperatura_maxima >= 42, TRUE, FALSE))
toRegress <- toRegress %>%  mutate(temp_43 = ifelse(temperatura_maxima >= 43, TRUE, FALSE))
toRegress <- toRegress %>%  mutate(temp_44 = ifelse(temperatura_maxima >= 44, TRUE, FALSE))


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
                fml = taxa_mortalidade ~  temp_37 | DTOBITO)

model_name <- paste0('Temperatura 37 ')
resultados_modelos[[model_name]] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  temp_38 | DTOBITO)

model_name <- paste0('Temperatura 38 ')
resultados_modelos[[model_name]] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  temp_39 | DTOBITO)

model_name <- paste0('Temperatura 39 ')
resultados_modelos[[model_name]] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  temp_40 | DTOBITO)

model_name <- paste0('Temperatura 40 ')
resultados_modelos[[model_name]] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  temp_41 | DTOBITO)

model_name <- paste0('Temperatura 41 ')
resultados_modelos[[model_name]] <- modelo

modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  temp_42 | DTOBITO)

model_name <- paste0('Temperatura 42 ')
resultados_modelos[[model_name]] <- modelo


modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  temp_43 | DTOBITO)

model_name <- paste0('Temperatura 43 ')
resultados_modelos[[model_name]] <- modelo


modelo <- feols(data = toRegress,
                fml = taxa_mortalidade ~  temp_44 | DTOBITO)

model_name <- paste0('Temperatura 44 ')
resultados_modelos[[model_name]] <- modelo

etable(resultados_modelos)