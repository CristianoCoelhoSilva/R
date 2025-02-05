options(scipen = 999)
library(sqldf)
library(ggrepel)
library(ggthemes)
library(ggplot2)
library(data.table)

dados <- read.csv("./DADOS/Brasil2.csv")

dados$Data <- as.Date(dados$data, format = "%Y-%m-%d")
dados$indice <- as.numeric(gsub(",", ".", dados$indice))
dados$valore <- as.factor(dados$valor)

index <- ggplot(dados, aes(x = Data, y = indice)) +
  geom_line(color = "blue", size = 0.5) +
  labs(title = "Série", x = "Data", y = "Valor") + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(facets = ~valor, scales = "free_y") +  
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

library(dplyr)
library(ggplot2)

# Calcular a diferença do logaritmo de 'indice'
dados <- dados %>%
  arrange(Data) %>%  # Garantir que os dados estão ordenados por Data
  mutate(diff_log_indice = c(NA, diff(log(indice))))  # Calcula a diferença logarítmica

# Plotando os dados com a transformação diff(log(indice))
index <- ggplot(dados, aes(x = Data, y = diff_log_indice, color = valore)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Série", x = "Data", y = "Diferença Log(Indice)") + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(facets = ~valor, scales = "free_y") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Data", y = "Diferença Log(Indice)")

# Exibindo o gráfico
print(index)

# Calcular a diferença logarítmica por grupo na coluna 'name'
dados <- dados %>%
  arrange(name, Data) %>%  # Garantir que os dados estão ordenados por 'name' e 'Data'
  group_by(name) %>%  # Agrupar os dados pela coluna 'name'
  mutate(diff_log_indice = c(NA, diff(log(indice)))) %>%  # Calcula a diferença logarítmica dentro de cada grupo
  ungroup()  # Desagrupar após a transformação

# Plotando os dados com a transformação diff(log(indice)) por grupo 'name'
index <- ggplot(dados, aes(x = Data, y = diff_log_indice, color = name)) +
  geom_line(size = 0.5, color = "blue", ) +
  labs(title = "Série", x = "Data", y = "Diferença Log(Indice)") + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(facets = ~name, scales = "free_y") +  
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
