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

lista_doencas <- c(
  'Influenza [gripe] e pneumonia',
  'Doenças crônicas das vias aéreas inferiores',
  #'Infecções agudas das vias aéreas superiores',
  'Doenças hipertensivas',
  'Doenças isquêmicas do coração',
  'Doenças cerebrovasculares',
  'Doenças reumáticas crônicas do coração',
  'Outras doenças bacterianas',
  'Outras formas de doença do coração'
)
resultados_modelos <- list()

for (doenca in lista_doencas) {
  
  
  #base <- death[death$causabas_capitulo == 'XX.  Causas externas de morbidade e mortalidade',]
  
  base <- death
  
  base <- base[base$causabas_grupo == doenca,]
  
  base <- base %>%
    mutate(
      ano = year(DTOBITO) 
    )
  
  ibge <- read_csv("TEMPERATURA/MORTALIDADE/ibge_2002_2019_comPopulacao.csv")
  ibge <- ibge %>%
    filter(ANO == 2019) %>% # Filtra somente as linhas onde a coluna 'Ano' é 2019
    filter(POPULACAO > 0 & PIB > 0) %>%
    mutate(IBGE = floor(IBGE / 10))
  
  toRegress =
    base %>%
    group_by(ano, DTOBITO, anomalia_max
             , idade_obito, def_sexo, def_raca_cor
             , CODMUNRES) %>%
    summarize(number_deaths = n()) %>%
    ungroup() %>%
    left_join(ibge %>%
                select(ano = ANO,
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
  
  names(toRegress) <- c("ano_obito", "data", "clima", "Age", "Gender", "Race", "Municipio", "number_deaths", "POPULACAO", "REGIAO", "Jovem","Idoso","Is_white","Is_male")
  
  toRegress$log_taxa_mortalidade <- ((100 *  toRegress$number_deaths) / toRegress$POPULACAO)
  toRegress$taxa_mortalidade <- (toRegress$number_deaths / toRegress$POPULACAO)
  
  modelo <- feols(data = toRegress
                  , fml = log_taxa_mortalidade ~  clima | data)
  
  
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
  
  resultados_modelos[[doenca_desc]] <- modelo}

etable(resultados_modelos)