library(lubridate)
library(lubridate)
library(stringr)
library(readr)
source("TEMPERATURA/Normais INPE.R")

lista_range_ano <- function(inicio, fim) {
  return(inicio:fim)
}

anos <- lista_range_ano(2008, 2023)


for (ano in anos) {

ano_desejado <- ano

normais <- normais_estacoes()

nome_arquivo_temperatura <- paste0("TEMPERATURA/DADOS/DADOS_INPE/ARQUIVOS/", ano_desejado, ".csv")
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
    by = c("codigo_estacao" = "id_estacao", "mes" = "mes"),
    relationship = "many-to-many"
  )

dados <- dados %>%
  mutate(
    flag_temp_maior = ifelse(temperatura_maxima > temp_maxima, 1, 0)
  )

dados <- dados %>%
  mutate(
    flag_temp_menor = ifelse(temperatura_minima < temp_minima, 1, 0)
  )

dados <- dados %>%
  mutate(
    diff_temp_maior = temperatura_maxima - temp_maxima
  )

dados <- dados %>%
  mutate(
    diff_temp_menor = temperatura_minima - temp_minima
  )

dados <- dados[c("data","mes","id","estacao","uf","codigo_estacao","temperatura_maxima","temperatura_minima","temp_maxima","temp_minima","flag_temp_maior","flag_temp_menor","diff_temp_maior","diff_temp_menor")]

colnames(dados)[colnames(dados) == "temp_maxima"] <- "temp_maxima_normais"
colnames(dados)[colnames(dados) == "temp_minima"] <- "temp_minima_normais"
colnames(dados)[colnames(dados) == "id"] <- "IBGE"


nome_arquivo_saida <- paste0('TEMPERATURA/DADOS/DADOS_TEMPERATURA_NORMAIS/TEMPERATURA_', ano_desejado, '.csv')
write.csv(dados, file = nome_arquivo_saida, row.names = FALSE, quote = TRUE, na = "")}