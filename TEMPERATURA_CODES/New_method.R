library(lubridate)
library(dplyr)

baseFinal <- base[base$DC_NOME %in% c("CURITIBA",
                                      "BELO HORIZONTE",
                                      "MANAUS",
                                      "NATAL",
                                      "PALMAS",
                                      "RIO DE JANEIRO",
                                      "BRASILIA",
                                      "FLORIANOPOLIS",
                                      "FORTALEZA",
                                      "GOIANIA",
                                      "PORTO ALEGRE",
                                      "SALVADOR",
                                      "SAO PAULO"), ]

ibge <- read_csv("TEMPERATURA/MUNICIPIOS/ibge_2002_2023_comPopulacao.csv")

baseFinal =
  baseFinal %>%
  group_by(DC_NOME,ano_obito,temperatura_maxima,DTOBITO,CODMUNRES) %>%
  summarize(number_deaths = n()) %>%
  ungroup() %>%
  inner_join(ibge %>%
               select(ano_obito = ANO,
                      CODMUNRES = IBGE,
                      POPULACAO
               ))

baseFinal$CODMUNRES <- NULL

baseFinal$woy <- week(baseFinal$DTOBITO)

DATATABLE <- read.csv("TEMPERATURA/PAPERS/data2023.csv")
DATATABLE <- DATATABLE %>% group_by(year, woy, date) %>% count()



baseFinal <- baseFinal %>%
  inner_join(DATATABLE %>%
               rename(ano_obito = year, woy = woy) %>% # Rename columns in DATATABLE BEFORE joining
               select(ano_obito, woy, date), # Select the columns you need from DATATABLE
             by = c("ano_obito", "woy")) # Specify the columns to join by


baseFinal$DTOBITO <- NULL

baseFinal <- baseFinal %>%
  group_by(DC_NOME, ano_obito, POPULACAO, woy, date) %>%
  summarise(
    mean_temperatura = mean(temperatura_maxima, na.rm = TRUE),
    total_deaths = sum(number_deaths, na.rm = TRUE)
  )

baseFinal <- baseFinal[c(1,2,4,6,3,7,5)]

colnames(baseFinal) <- c('location','year','woy','temp','popu','mort','date')

write.csv(baseFinal, "DATATABLEBRAZIL.csv")
