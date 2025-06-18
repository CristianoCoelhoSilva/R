library(readr)
library(dplyr)
library(lubridate)
library(readxl) # Necessário para read_excel se a função anterior não for chamada
library(tidyr)  # Necessário para pivot_longer/wider se a função anterior não for chamada
library(writexl) # Necessário para write_xlsx
library(readr)
library(dplyr)
library(purrr) # Para a função map_df

rm(list = setdiff(ls(), "temperatura"))

pasta_de_arquivos <- 'TEMPERATURA/MORTALIDADE/ESTUDO'

todos_os_arquivos <- list.files(path = pasta_de_arquivos, pattern = "\\.csv$", full.names = TRUE)

mortalidade <- map_df(todos_os_arquivos, read_csv)
mortalidade <- mortalidade[!mortalidade$causabas_capitulo %in% c('X. Doenças do aparelho respiratório'), ]
mortalidade <- mortalidade[mortalidade$CODMUNRES == 530010, ]

indices_7_digitos <- nchar(as.character(mortalidade$DTOBITO)) == 7
mortalidade$DTOBITO[indices_7_digitos] <- paste0("0", mortalidade$DTOBITO[indices_7_digitos])
mortalidade$DTOBITO <- as.Date(as.character(mortalidade$DTOBITO), format = "%d%m%Y")

mortalidade <- mortalidade[c('DTOBITO','CODMUNRES','ano_obito')]

ibge <- read_csv("TEMPERATURA/MUNICIPIOS/ibge_2002_2019_comPopulacao.csv")

ibge <- ibge %>%
  filter(POPULACAO > 0 & PIB > 0) %>%
  mutate(IBGE = floor(IBGE / 10))

tudo =
  mortalidade %>%
  group_by(ano_obito, DTOBITO, CODMUNRES) %>%
  summarize(number_deaths = n()) %>%
  ungroup() %>%
  left_join(ibge %>%
              select(ano_obito = ANO,
                     CODMUNRES = IBGE,
                     POPULACAO
              ))

tudo =
  tudo %>%
  mutate(taxa_mortes_diaria = 100000 * number_deaths / POPULACAO)

tudo$mes_dia <- format(tudo$DTOBITO, "%m-%d")

tudo <- tudo[c('DTOBITO','ano_obito','mes_dia','POPULACAO','number_deaths')]

temperatura$Codigo_IBGE <- substr(temperatura$Codigo_IBGE, 1, 6)
temperatura$Codigo_IBGE <- as.numeric(temperatura$Codigo_IBGE)
temperatura <- temperatura[temperatura$Codigo_IBGE == 530010, ]

temperatura <- temperatura[c('DATA','Y30')]

tudo =
  tudo %>%
  inner_join(temperatura %>%
              select(DTOBITO = DATA,
                     Y30))

rm(list = setdiff(ls(), "tudo"))

tudo_mortes_esperadas <- tudo %>%
  filter(!Y35) %>% # Excluir os dias de HW para o cálculo da base
  group_by(mes_dia) %>%
  summarise(
    mortes_esperadas_dia = mean(number_deaths / POPULACAO * 100000, na.rm = TRUE), # Taxa média
    .groups = 'drop'
  )


tudo <- left_join(tudo, tudo_mortes_esperadas, by = "mes_dia")

tudo$mortes_esperadas <- tudo$mortes_esperadas_dia * tudo$POPULACAO / 100000


tudo_hw_eventos_OE <- tudo %>%
  arrange(DTOBITO) %>% 
  filter(Y30) %>% 
  mutate(
    group_start = (DTOBITO - lag(DTOBITO, default = first(DTOBITO) - 1)) != 1
  ) %>%
  mutate(
    # 'hw_event_id' é um ID sequencial para cada evento de onda de calor
    hw_event_id = cumsum(group_start)
  ) %>%
  group_by(hw_event_id) %>% # Agora agrupa por cada evento de onda de calor
  summarise(
    data_inicio_hw = min(DTOBITO),
    data_fim_hw = max(DTOBITO),
    duracao_dias = n(),
    mortes_observadas_hw = sum(number_deaths),
    mortes_esperadas_hw = sum(mortes_esperadas),
    # Calcula a Razão O/E para o evento completo da onda de calor
    razao_OE_hw_evento = mortes_observadas_hw / mortes_esperadas_hw,
    .groups = 'drop' # Remove o agrupamento após o summarise
  )

print("Razão O/E para cada evento de Onda de Calor:")
print(tudo_hw_eventos_OE)

tudo_hw_dias_individuais <- tudo %>%
  filter(Y30 == TRUE) %>%
  mutate(
    razao_OE = number_deaths / mortes_esperadas
  )

print("Razão O/E por dia individual de Onda de Calor:")
print(tudo_hw_dias_individuais %>% select(DTOBITO, number_deaths, mortes_esperadas, razao_OE))
