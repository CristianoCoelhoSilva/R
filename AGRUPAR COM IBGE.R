library(dplyr)
library(data.table)

dados <- dados %>%
  mutate(codmunres = case_when(
    nchar(as.character(codmunres)) == 7 ~ substr(as.character(codmunres), 2, 7),
    TRUE ~ as.character(codmunres)
  ),
codmunres = as.numeric(codmunres) # Converte para numérico
)

ibge = 
  fread("TEMPERATURA/ibge_2002_2019_comPopulacao.csv") %>% 
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
  dados %>% 
  mutate(discrete_idade_obito = dplyr::ntile(idade_obito, 10)) %>% 
  group_by(ano_obito, codmunres, discrete_idade_obito, def_sexo, def_raca_cor, def_escol, causabas_capitulo) %>% 
  summarize(number_deaths = n()) %>% 
  ungroup() %>% 
  left_join(ibge %>% 
              select(ano_obito = ANO,
                     codmunres = ibge2,
                     PIB_PER_CAPITA,
                     POPULACAO,
                     NOME
              ))

toRegress = 
  tudo %>% 
  mutate(frac_deaths = 100 * number_deaths / POPULACAO)


df_agrupado <- toRegress %>%
  group_by(NOME) %>%
  summarise(count = n())
