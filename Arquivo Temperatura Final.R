library(lubridate)
library(lubridate)
library(stringr)

source("TEMPERATURA/Normais.R")

normais <- normais_estacoes(FALSE)

nome_arquivo_temperatura <- paste0("TEMPERATURA/DADOS/DADOS_INPE/ARQUIVOS/", 2023, ".csv")
dados <- read_csv(nome_arquivo_temperatura)

dados <- dados %>%
  mutate(temperatura_maxima = na_if(temperatura_maxima, -9999),
         temperatura_maxima = na_if(temperatura_maxima, Inf),
         temperatura_maxima = na_if(temperatura_maxima, -Inf))

dados <- dados %>%
  mutate(temperatura_minima = na_if(temperatura_minima, -9999),
         temperatura_minima = na_if(temperatura_minima, Inf),
         temperatura_minima = na_if(temperatura_minima, -Inf))

dados <- dados %>%
  filter(!is.na(temperatura_maxima), !is.na(temperatura_minima))


dados <- dados %>%
  mutate(
    Mes_lower = month(DATA, label = TRUE, abbr = FALSE),
    Mes = str_to_title(Mes_lower)
  ) %>%
  select(-Mes_lower) # Remove a coluna temporária com letras minúsculas

dados <- dados %>%
  rename_with(tolower) %>%
  inner_join(
    normais %>% rename_with(tolower),
    by = c("codigo_estacao" = "id_estacao", "mes" = "mês"),
    relationship = "many-to-many"
  )

dados <- dados %>%
  mutate(
    flag_temp_maior = ifelse(temperatura_maxima > maxima, 1, 0)
  )

dados <- dados %>%
  mutate(
    flag_temp_menor = ifelse(temperatura_minima < minima, 1, 0)
  )

dados <- dados %>%
  mutate(
    diff_temp_maior = temperatura_maxima - maxima
  )

dados <- dados %>%
  mutate(
    diff_temp_menor = temperatura_minima - minima
  )