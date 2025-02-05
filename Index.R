options(scipen = 999)
library(ggrepel)
library(ggthemes)
library(ggplot2)
library(data.table)
library(readxl)
library(tidyr)
library(dplyr)

# Definir o caminho para o arquivo Excel
arquivo_excel <- "DADOS/setores_brasil.xlsx"

# Ler a planilha (caso você tenha mais de uma planilha, especifique o nome ou índice da planilha)
dados <- read_excel(arquivo_excel)

dados <- dados %>%
  pivot_longer(cols = starts_with("MXBR"),  # Seleciona as colunas que começam com "MXBR"
               names_to = "Indice",         # Cria a coluna "Indice" com o nome das colunas antigas
               values_to = "Valor")        # Cria a coluna "Valor" com os valores

dados$Data <- as.Date(dados$dates, format = "%Y-%m-%d")
dados$Valor <- as.numeric(gsub(",", ".", dados$Valor))

index <- ggplot(dados, aes(x = Data, y = Valor)) +
  geom_line(color = "blue", size = 0.5) +
  labs(title = "Série", x = "Data", y = "Valor") + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(facets = ~Indice, scales = "free_y") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="Data", y="Valor")

index

ggsave(
  path = "./IMAGES",
  filename = "serie.pdf",
  device = "pdf",
  width = 15,
  height = 9,
  dpi = 640
)

# Calcular a diferença logarítmica por grupo na coluna 'Indice'
dados <- dados %>%
  arrange(Indice, Data) %>%  # Garantir que os dados estão ordenados por 'name' e 'Data'
  group_by(Indice) %>%  # Agrupar os dados pela coluna 'name'
  mutate(diff_log_indice = c(NA, diff(log(Valor)))) %>%  # Calcula a diferença logarítmica dentro de cada grupo
  ungroup()  # Desagrupar após a transformação

# Plotando os dados com a transformação diff(log(indice)) por grupo 'name'
index <- ggplot(dados, aes(x = Data, y = diff_log_indice, color = Indice)) +
  geom_line(size = 0.5, color = "blue") +
  labs(title = "Série", x = "Data", y = "Diferença Log(Indice)") + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(facets = ~Indice, scales = "free_y") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Data", y = "Diferença Log(Indice)")

# Exibindo o gráfico
print(index)

ggsave(
  path = "./IMAGES",
  filename = "serie_log.pdf",
  device = "pdf",
  width = 15,
  height = 9,
  dpi = 640
)
