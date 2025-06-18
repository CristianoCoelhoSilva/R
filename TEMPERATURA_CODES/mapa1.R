library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Obter os dados geoespaciais do Brasil (necessário para que o geom_sf possa desenhar a base)
brasil_sf <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")

# Dados de Brasília
brasilia_data <- data.frame(
  capital = "Brasília",
  lat = -15.78,
  long = -47.92,
  temperatura = 26 # Exemplo de temperatura
)

# Criar o gráfico focado em Brasília
ggplot(brasil_sf) +
  geom_sf(fill = "white", color = "black") + # Mapa base com fundo branco e contorno preto
  geom_point(data = brasilia_data, aes(x = long, y = lat, color = temperatura),
             size = 4, shape = 19) + # Ponto de Brasília, colorido pela temperatura
  scale_color_gradient2(low = "blue", mid = "yellow", high = "red",
                        midpoint = brasilia_data$temperatura, name = "Temperatura (°C)") + # Escala de cores para a temperatura
  geom_text(data = brasilia_data,
            aes(x = long, y = lat, label = paste0(capital, " (", temperatura, "°C)")),
            vjust = 0, hjust = 0.5, size = 4, nudge_y = -0.7, color = "black") + # Rótulo da capital com temperatura
  coord_sf(xlim = c(-48.5, -47.3), ylim = c(-16.2, -15.3), expand = FALSE) + # **Define o foco do mapa em Brasília**
  labs(title = "Mapa de Brasília com Temperatura", # Título do mapa
       x = "Longitude", # Rótulo do eixo X
       y = "Latitude", # Rótulo do eixo Y
       caption = "Temperaturas são valores de exemplo.") + # Legenda de rodapé
  theme_bw() + # Tema em preto e branco para o gráfico
  theme(legend.position = "right") # Posição da legenda da temperatura