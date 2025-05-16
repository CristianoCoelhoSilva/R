library(dplyr)
library(lubridate)

# Seu dataframe de exemplo (substitua pelo seu dataframe real)
dados <- read_csv("TEMPERATURA/DADOS/DADOS_TEMPERATURA_NORMAIS/TEMPERATURA_2008_2019.csv")

dados <- dados %>%
  mutate(anomalia_max = ifelse(diff_temp_maior >= 5, 1, 0)) %>%
  mutate(anomalia_min = ifelse(diff_temp_menor <= -5, 1, 0))

dados <- dados %>%
  filter(!is.na(temp_maxima_normais), !is.na(temp_minima_normais))

#dados <- dados %>% filter(estacao == 'SANTA MARIA MADALENA')

# Obtém a lista de cidades únicas
cidades <- unique(dados$estacao)

# Cria um dataframe vazio para armazenar os resultados finais
dados_final <- data.frame()

# Loop para cada cidade
for (cidade in cidades) {
  # Filtra o dataframe para a cidade atual
  dados_cidade <- dados %>% filter(estacao == cidade)
  
  # Calcula as ondas de calor e frio para a cidade
  dados_cidade <- dados_cidade %>%
    mutate(
      consecutivos_max = (anomalia_max == 1),
      grupo_max = cumsum(consecutivos_max != lag(consecutivos_max, default = FALSE))
    ) %>%
    group_by(estacao, grupo_max) %>%
    mutate(n_consecutivos_max = sum(consecutivos_max)) %>%
    ungroup() %>%
    mutate(onda_calor = ifelse(n_consecutivos_max >= 5 & anomalia_max == 1, 1, 0)) %>%
    select(-consecutivos_max, -grupo_max, -n_consecutivos_max) %>%
    mutate(
      consecutivos_min = (anomalia_min == 1),
      grupo_min = cumsum(consecutivos_min != lag(consecutivos_min, default = FALSE))
    ) %>%
    group_by(estacao, grupo_min) %>%
    mutate(n_consecutivos_min = sum(consecutivos_min)) %>%
    ungroup() %>%
    mutate(onda_frio = ifelse(n_consecutivos_min >= 5 & anomalia_min == 1, 1, 0)) %>%
    select(-consecutivos_min, -grupo_min, -n_consecutivos_min)

  # Adiciona os dados da cidade ao dataframe final
  dados_final <- bind_rows(dados_final, dados_cidade)
  
}

nome_arquivo_saida <- paste0('TEMPERATURA/DADOS/BASE_FINAL_TEMPERATURA', '.csv')
write.csv(dados_final, file = nome_arquivo_saida, row.names = FALSE, quote = TRUE, na = "")