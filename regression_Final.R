rm(list = ls())
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
#source("./Script/plot_utils.R")
set.seed('123')

base <- read_csv("TEMPERATURA/DADOS/BASE_FINAL.csv")
base <- base[base$causabas_capitulo == 'XX.  Causas externas de morbidade e mortalidade',]
base <- base[base$causabas_grupo == 'Agressões',]  
base$def_raca_cor %>% table()

ibge = 
  fread("TEMPERATURA/DADOS/ibge_2002_2019_comPopulacao.csv") %>% 
  filter(POPULACAO > 0 & PIB > 0) %>%
  mutate(PIB_PER_CAPITA = PIB / POPULACAO) %>% 
  mutate(ibge2 = floor(IBGE / 10)) %>%
  group_by(IBGE) %>%
  arrange(ANO) %>% 
  mutate(lag_PIB = dplyr::lag(PIB, n = 1),
         lag_PIB_PER_CAPITA = dplyr::lag(PIB_PER_CAPITA, n = 1)) %>% 
  ungroup() %>% 
  mutate(growth_pib = log(PIB) - log(lag_PIB),
         growth_pib_per_capita = log(PIB_PER_CAPITA) - log(lag_PIB_PER_CAPITA)) 

tudo = 
  base %>% 
  #  mutate(DTOBITO = dmy(DTOBITO)) %>% 
  #  mutate(MES_ANO = year(DTOBITO)*1e2 + month(DTOBITO)) %>% 
  mutate(discrete_idade_obito = dplyr::ntile(idade_obito, 10)) %>% 
  group_by(ano_obito, data, anomalia_max
         , discrete_idade_obito, def_sexo, def_raca_cor
         , IBGE) %>% 
  summarize(number_deaths = n()) %>% 
  ungroup() %>% 
  left_join(ibge %>% 
              select(ano_obito = ANO,
                     IBGE = ibge2,
                     POPULACAO,
                     REGIAO
              ))

toRegress = 
  tudo %>% 
  mutate(frac_deaths = 100 * number_deaths / POPULACAO)

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

names(toRegress) <- c("ano_obito", "data", "clima", "Age", "Gender", "Race", "Municipio", "number_deaths", "POPULACAO", "REGIAO", "frac_deaths", "is_white","is_male")

toRegress$log_taxa_mortalidade <- log(toRegress$number_deaths / toRegress$POPULACAO)

modelo <- feols(data = toRegress
                ,fml = log_taxa_mortalidade ~ is_white + is_male + Age + clima | data)

etable(modelo)
