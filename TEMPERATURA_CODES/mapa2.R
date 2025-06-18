library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Obter os dados geoespaciais do Brasil
brasil_sf <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")

# Dados de TODAS as Capitais do Brasil com TEMPERATURAS (valores de exemplo)
capitals_data <- data.frame(
  capital = c(
    "Rio Branco", "Maceió", "Macapá", "Manaus", "Salvador", "Fortaleza",
    "Vitória", "Goiânia", "São Luís", "Cuiabá", "Campo Grande", "Belo Horizonte",
    "Belém", "João Pessoa", "Curitiba", "Recife", "Teresina", "Rio de Janeiro",
    "Natal", "Porto Alegre", "Porto Velho", "Boa Vista", "Florianópolis",
    "São Paulo", "Aracaju", "Palmas", "Brasília"
  ),
  lat = c(
    -9.97, -9.67, 0.03, -3.11, -12.97, -3.73, -20.31, -16.68, -2.53, -15.60,
    -20.44, -19.91, -1.45, -7.12, -25.43, -8.05, -5.09, -22.90, -5.78, -30.03,
    -8.76, 2.82, -27.59, -23.55, -10.94, -10.25, -15.78
  ),
  long = c(
    -67.81, -35.74, -51.07, -60.02, -38.50, -38.52, -40.34, -49.25, -44.30, -56.10,
    -54.64, -43.94, -48.49, -34.86, -49.27, -34.88, -42.80, -43.17, -35.21, -51.23,
    -63.90, -60.67, -48.55, -46.63, -37.07, -48.33, -47.92
  ),
  temperatura = c(
    30, 28, 29, 31, 29, 29,
    25, 27, 29, 32, 28, 26,
    30, 28, 20, 27, 30, 24,
    28, 18, 30, 31, 21,
    22, 28, 32, 26
  )
)

# Criar o gráfico com as cores dos pontos baseadas na temperatura
ggplot(brasil_sf) +
  geom_sf(fill = "white", color = "black") + # Fundo do mapa em verde bandeira
  geom_point(data = capitals_data, aes(x = long, y = lat, color = temperatura),
             size = 3, shape = 19) + # Cor do ponto mapeada pela temperatura
  scale_color_gradient2(
    low = "lightblue",     # Azul mais escuro (frio)
    mid = "#FFDE00",     # Amarelo (cores da bandeira)
    high = "#FF0000",    # Vermelho (quente)
    midpoint = mean(capitals_data$temperatura), # Ponto central para a transição de cores
    name = "Temperatura (°C)" # Legenda da escala de cores
  ) +
  geom_text(data = capitals_data,
            aes(x = long, y = lat, label = paste0(capital, " (", temperatura, "°C)")),
            vjust = 0, hjust = 0.5, size = 3, check_overlap = TRUE,
            nudge_y = -1.2,
            color = "black") + # Cor do texto preto para melhor legibilidade
  coord_sf(xlim = c(-75, -30), ylim = c(-35, 10), expand = FALSE) +
  labs(title = "Mapa do Brasil com Capitais e Temperaturas por Cor (Fundo Verde)",
       x = "Longitude", y = "Latitude",
       caption = "Temperaturas são valores de exemplo.") +
  theme_bw()
