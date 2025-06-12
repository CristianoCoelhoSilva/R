library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr) # Para manipulação de dados
library(readr) # Para ler dados de texto de forma mais robusta

# Obter os dados geoespaciais do Brasil
brasil_sf <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")

temperatura_data_raw_text <- validacao

temperatura_data_raw_text <- temperatura_data_raw_text[temperatura_data_raw_text$ano_obito %in% c(2023), ]

temperatura_data_raw <- temperatura_data_raw_text

# --- 2. Preparar os dados de coordenadas das capitais ---
# É crucial que os nomes das capitais correspondam entre os dois datasets
capitals_coords <- data.frame(
  DC_NOME = toupper(c( # Convertendo para maiúsculas para corresponder aos dados fornecidos
    "Rio Branco", "Maceió", "Macapá", "Manaus", "Salvador", "Fortaleza",
    "Vitória", "Goiânia", "São Luís", "Cuiabá", "Campo Grande", "Belo Horizonte",
    "Belém", "João Pessoa", "Curitiba", "Recife", "Teresina", "Rio de Janeiro",
    "Natal", "Porto Alegre", "Porto Velho", "Boa Vista", "Florianópolis",
    "São Paulo", "Aracaju", "Palmas", "Brasília"
  )),
  lat = c(
    -9.97, -9.67, 0.03, -3.11, -12.97, -3.73, -20.31, -16.68, -2.53, -15.60,
    -20.44, -19.91, -1.45, -7.12, -25.43, -8.05, -5.09, -22.90, -5.78, -30.03,
    -8.76, 2.82, -27.59, -23.55, -10.94, -10.25, -15.78
  ),
  long = c(
    -67.81, -35.74, -51.07, -60.02, -38.50, -38.52, -40.34, -49.25, -44.30, -56.10,
    -54.64, -43.94, -48.49, -34.86, -49.27, -34.88, -42.80, -43.17, -35.21, -51.23,
    -63.90, -60.67, -48.55, -46.63, -37.07, -48.33, -47.92
  )
)

# --- 3. Combinar os dados de temperatura com as coordenadas ---
# Fazendo um left_join para manter todas as capitais e adicionar as temperaturas
capitals_temperatura_anual <- left_join(capitals_coords, temperatura_data_raw, by = "DC_NOME")

# Remover linhas onde não há dados de temperatura (se alguma capital não estiver no seu CSV)
capitals_temperatura_anual <- na.omit(capitals_temperatura_anual)

# --- 4. Criar o gráfico com facetas por ano ---
ggplot(brasil_sf) +
  geom_sf(fill = "white", color = "black") + # Fundo do mapa em verde bandeira
  geom_point(data = capitals_temperatura_anual, aes(x = long, y = lat, color = media_temperatura_maxima),
             size = 3, shape = 19) + # Cor do ponto mapeada pela temperatura máxima
  scale_color_gradient2(
    low = "lightblue",     # Azul mais escuro (frio)
    mid = "#FFDE00",     # Amarelo (cores da bandeira)
    high = "#FF0000",    # Vermelho (quente)
    # O midpoint é calculado com base em *todos* os dados de temperatura para uma escala consistente
    midpoint = mean(capitals_temperatura_anual$media_temperatura_maxima, na.rm = TRUE),
    name = "Max. Average Temp. (°C)" # Legenda da escala de cores
  ) +
  geom_text(data = capitals_temperatura_anual,
            aes(x = long, y = lat, label = DC_NOME), # Apenas o nome da capital
            vjust = 0, hjust = 0.5, size = 2.5, check_overlap = TRUE,
            nudge_y = -1.2, # Ajustei o nudge_y para maior afastamento
            color = "black") + # Cor do texto preto para melhor legibilidade
  coord_sf(xlim = c(-75, -30), ylim = c(-35, 10), expand = FALSE) +
  # Adicionar facetas por ano de óbito
  facet_wrap(~ ano_obito, ncol = 2) +
  theme_bw() +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "white"), # Azul escuro para o "mar" (fundo do painel)
        plot.background = element_rect(fill = "white"), # Fundo do plot também azul
        strip.background = element_rect(fill = "white"), # Cor de fundo das faixas de ano (amarelo)
        strip.text = element_text(color = "black", face = "bold"), # Cor e estilo do texto nas faixas
        plot.title = element_text(hjust = 0.5), # Centraliza o título principal
        plot.subtitle = element_text(hjust = 0.5)) # Centraliza o subtítulo
