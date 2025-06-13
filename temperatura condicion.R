

# Certifique-se de ter o tidyverse (ou pelo menos dplyr e forcats) instalado e carregado
# install.packages("tidyverse") # Se não tiver instalado ainda
library(dplyr)
library(forcats) # Parte do tidyverse, útil para manipular fatores
library(ggplot2)

# Assumindo que 'base' é o seu dataframe com as colunas 'n' e 'causabas_capitulo'
# df_diseases <- base # Esta linha já foi feita por você

# --- Passos para ordenar o dataframe e o fator (AGORA EM ORDEM CRESCENTE) ---

# 1. Ordenar o dataframe df_diseases pelo 'n' em ordem CRESCENTE
df_diseases <- df_diseases %>%
  arrange(n) # Apenas 'n' para ordem crescente

# 2. Converter 'causabas_capitulo' para fator e definir a ordem dos níveis
#    usando a ordem recém-definida pelo 'n' (do menor para o maior)
df_diseases$causabas_capitulo <- factor(df_diseases$causabas_capitulo,
                                        levels = df_diseases$causabas_capitulo)

# --- Código do ggplot (mantido como antes, com as fontes maiores) ---

ggplot(df_diseases, aes(x = n, y = causabas_capitulo)) +
  geom_col(fill = 'steelblue') +
  labs(
    title = '', # Título ajustado
    x = '',
    y = ''
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),   # Aumenta o título do gráfico
    axis.title = element_text(size = 14),              # Aumenta os títulos dos eixos
    axis.text = element_text(size = 12),               # Aumenta os rótulos (nomes das doenças e números) dos eixos
    legend.title = element_text(size = 14),            # Se houver legenda, aumenta o título
    legend.text = element_text(size = 12)              # Se houver legenda, aumenta o texto
  )