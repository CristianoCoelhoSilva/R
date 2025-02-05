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

index <- ggplot(dados, aes(x = Data, y = indice, color = valore)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Série", x = "Data", y = "Valor") + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(facets = ~valor, scales = "free_y") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="Data", y="Valor")

index

ggsave(
  path = "./IMAGES",
  filename = "serie.png",
  device = "png",
  width = 9,
  height = 6,
  dpi = 320
)

library(dplyr)
library(ggplot2)

# Aplicando logaritmo na variável 'indice'
dados <- dados %>%
  mutate(indice_log = log(indice))

# Plotando os dados com a variável logaritmada
index <- ggplot(dados, aes(x = Data, y = indice_log, color = valore)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Série", x = "Data", y = "Log(Valor)") + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(facets = ~valor, scales = "free_y") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Data", y = "Log(Valor)")

# Exibindo o gráfico
print(index)
