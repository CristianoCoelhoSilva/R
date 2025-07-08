# Certifique-se de que o seu dataframe 'data' esteja carregado
# Exemplo de como o seu 'data' poderia ser (substitua pelo seu dataframe real):
# data <- data.frame(
#   Dummy_fl1 = sample(0:1, 100, replace = TRUE),
#   Dummy_fl2 = sample(0:1, 100, replace = TRUE),
#   Dummy_fl3 = sample(0:1, 100, replace = TRUE),
#   Dummy_fl4 = sample(0:1, 100, replace = TRUE),
#   Dummy_fl5 = sample(0:1, 100, replace = TRUE),
#   Dummy_fl6 = sample(0:1, 100, replace = TRUE),
#   Dummy_fl7 = sample(0:1, 100, replace = TRUE),
#   Dummy_fl8 = sample(0:1, 100, replace = TRUE)
# )

library(dplyr)
library(tidyr) # Para a função pivot_wider

# Lista das colunas Dummy
dummy_cols <- paste0("Dummy_fl", 1:8)

# Inicializa um dataframe vazio para armazenar os resultados
resultados_df <- data.frame()

# Itera sobre cada coluna e adiciona os resultados ao dataframe
for (col_name in dummy_cols) {
  if (col_name %in% names(data)) { # Verifica se a coluna existe no dataframe
    contagem <- data %>%
      group_by(!!sym(col_name)) %>%
      count() %>%
      rename(Valor = 1, Contagem = n) %>%
      mutate(Variavel = col_name) %>%
      select(Variavel, Valor, Contagem)
    
    resultados_df <- bind_rows(resultados_df, contagem)
  } else {
    warning(paste("A coluna", col_name, "não foi encontrada no dataframe 'data'."))
  }
}

resultados_df <- resultados_df %>%
  mutate(Valor_Label = ifelse(Valor == 0, "Não", "Sim")) %>% # Create new labels "Não" and "Sim"
  select(-Valor) %>% # Remove the original Valor column
  pivot_wider(
    names_from = Valor_Label, # Take values from Valor_Label to create new column names
    values_from = Contagem,   # Fill the new columns with values from Contagem
    values_fill = 0           # Fill NA values (if a combination doesn't exist) with 0
  )

print(resultados_df)