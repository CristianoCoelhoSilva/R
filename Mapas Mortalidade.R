library(readr)
library(dplyr)
library(lubridate)
library(readxl) # Necessário para read_excel se a função anterior não for chamada
library(tidyr)  # Necessário para pivot_longer/wider se a função anterior não for chamada
library(writexl) # Necessário para write_xlsx
library(readr)
library(dplyr)
library(purrr) # Para a função map_df
library(sqldf)

pasta_de_arquivos <- 'TEMPERATURA/MORTALIDADE/ESTUDO'

todos_os_arquivos <- list.files(path = pasta_de_arquivos, pattern = "\\.csv$", full.names = TRUE)

mortalidade <- map_df(todos_os_arquivos, read_csv)

#mortalidade <- mortalidade[mortalidade$ano_obito == 2002, ]

mortalidade <- mortalidade[c(2,5,6,8,12,13,14)]


base = sqldf("select CODMUNRES
                   , ano_obito
                   , idade_obito
                   , case
                      when causabas_capitulo = 'I.   Algumas doenças infecciosas e parasitárias' then  'I.   Certain infectious and parasitic diseases'
                      when causabas_capitulo = 'II.  Neoplasias (tumores)' then 'II.  Neoplasms'
                      when causabas_capitulo = 'III. Doenças sangue órgãos hemat e transt imunitár' then 'III. Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism'
                      when causabas_capitulo = 'IV.  Doenças endócrinas nutricionais e metabólicas' then 'IV.  Endocrine, nutritional and metabolic diseases'
                      when causabas_capitulo = 'IX.  Doenças do aparelho circulatório' then 'IX.  Diseases of the circulatory system'
                      when causabas_capitulo = 'V.   Transtornos mentais e comportamentais' then 'V.   Mental and behavioural disorders'
                      when causabas_capitulo = 'VI.  Doenças do sistema nervoso' then 'VI.  Diseases of the nervous system'
                      when causabas_capitulo = 'VII. Doenças do olho e anexos' then 'VII. Diseases of the eye and adnexa'
                      when causabas_capitulo = 'VIII.Doenças do ouvido e da apófise mastóide' then 'VIII.Diseases of the ear and mastoid process'
                      when causabas_capitulo = 'X.   Doenças do aparelho respiratório' then 'X.   Diseases of the respiratory system'
                      when causabas_capitulo = 'XI.  Doenças do aparelho digestivo' then 'XI.  Diseases of the digestive system'
                      when causabas_capitulo = 'XII. Doenças da pele e do tecido subcutâneo' then 'XII. Diseases of the skin and subcutaneous tissue'
                      when causabas_capitulo = 'XIII.Doenças sist osteomuscular e tec conjuntivo' then 'XIII.Diseases of the musculoskeletal system and connective tissue'
                      when causabas_capitulo = 'XIV. Doenças do aparelho geniturinário' then 'XIV. Diseases of the genitourinary system'
                      when causabas_capitulo = 'XV.  Gravidez parto e puerpério' then 'XV.  Pregnancy, childbirth and the puerperium'
                      when causabas_capitulo = 'XVI. Algumas afec originadas no período perinatal' then 'XVI. Certain conditions originating in the perinatal period'
                      when causabas_capitulo = 'XVII.Malf cong deformid e anomalias cromossômicas' then 'XVII.Congenital malformations, deformations and chromosomal abnormalities'
                      when causabas_capitulo = 'XVIII.Sint sinais e achad anorm ex clín e laborat' then 'XVIII.Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified'
                      when causabas_capitulo = 'XX.  Causas externas de morbidade e mortalidade' then 'XX.  External causes of morbidity and mortality'
                     end causabas_capitulo
                from mortalidade")

base <- base %>% group_by(causabas_capitulo) %>% count()


print(base)

mortalidade$CODMUNRES <- ifelse(
  nchar(mortalidade$CODMUNRES) == 7,           # Condição: se o número de caracteres for igual a 7
  substr(mortalidade$CODMUNRES, 1, 6),       # Se verdadeiro: pega os 6 primeiros caracteres
  mortalidade$CODMUNRES                      # Se falso: mantém o valor original
)

mortalidade$CODMUNRES <- as.numeric(mortalidade$CODMUNRES)


ibge <- read_csv("TEMPERATURA/MUNICIPIOS/ibge_2002_2019_comPopulacao.csv")

ibge$IBGE <- ifelse(
  nchar(ibge$IBGE) == 7,           # Condição: se o número de caracteres for igual a 7
  substr(ibge$IBGE, 1, 6),       # Se verdadeiro: pega os 6 primeiros caracteres
  ibge$IBGE                      # Se falso: mantém o valor original
)

ibge$IBGE <- as.numeric(ibge$IBGE)

tudo =
  mortalidade %>%
  group_by(CODMUNRES, ano_obito) %>%
  summarize(number_deaths = n()) %>%
  ungroup() %>%
  inner_join(ibge %>%
               select(ano_obito = ANO,
                      CODMUNRES = IBGE,
                      POPULACAO,
                      REGIAO
               ))


tudo =
  tudo %>%
  mutate(taxa_mortalidade = log(1+number_deaths))





