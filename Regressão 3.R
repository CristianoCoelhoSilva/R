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
library(readr)

set.seed('123')

library(dplyr)
library(readr)

lista_doencas <- c(
  "IX.  Doenças do aparelho circulatório",
  "X.   Doenças do aparelho respiratório",
  "XVII.Malf cong deformid e anomalias cromossômicas",
  "II.  Neoplasias (tumores)",
  "VI.  Doenças do sistema nervoso",
  "XI.  Doenças do aparelho digestivo",
  "XII. Doenças da pele e do tecido subcutâneo",
  "XX.  Causas externas de morbidade e mortalidade"
)
resultados_modelos <- list()

for (doenca in lista_doencas) {
  
  base <- read_csv("TEMPERATURA/DADOS/BASE_FINAL.csv")
  base <- base[base$causabas_capitulo == doenca,]

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
                  ,fml = log_taxa_mortalidade ~ is_white + is_male + Age + clima | data
  )
  
  doenca_desc <- case_when(
    doenca == "IX.  Doenças do aparelho circulatório" ~ "Circulatório",
    doenca == "X.   Doenças do aparelho respiratório" ~ "Respiratório",
    doenca == "XVII.Malf cong deformid e anomalias cromossômicas" ~ "Anomalias Cromos.",
    doenca == "II.  Neoplasias (tumores)" ~ "Neoplasias",
    doenca == "VI.  Doenças do sistema nervoso" ~ "Sistema Nervoso",
    doenca == "XI.  Doenças do aparelho digestivo" ~ "Aparelho Digestivo",
    doenca == "XII. Doenças da pele e do tecido subcutâneo" ~ "Pele e Tecido Sub.",
    doenca == "VII. Doenças do olho e anexos" ~ "Olho e Anexos",
    doenca == "XX.  Causas externas de morbidade e mortalidade" ~ "Morbidade e Mortalidade",
    TRUE ~ doenca # Mantém a descrição original se não houver correspondência
  )
  
  resultados_modelos[[doenca_desc]] <- modelo
}

  etable(resultados_modelos)
