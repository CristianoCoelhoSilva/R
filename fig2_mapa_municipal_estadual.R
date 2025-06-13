library(sf)
library(dplyr)
library(ggplot2)
library(psych)
library(scales) 
library(stringr)

shpMun <- st_read("PROJETO/PROJETO/DADOS/IBGE/BRMUE250GC_SIR.shp")
shpUFs <- st_read("PROJETO/PROJETO/DADOS/IBGE/BRUFE250GC_SIR.shp")

#Municipios <- tudo[c("CODMUNRES", "ano_obito", "taxa_mortalidade")]
#Municipios <- Municipios[Municipios$ano_obito == 2002, ]

Municipios <- validacao[c("Codigo_IBGE","Ano","media_temperatura_maxima")]
Municipios <- Municipios[Municipios$Ano == 2004, ]

# --- 3. Processar `CODMUNRES` e `CD_GEOCMU` para junção ---
# Converte CODMUNRES para caractere (importante para manipulação de string)
Municipios$Codigo_IBGE <- as.character(Municipios$Codigo_IBGE)

# APlica a sua regra: se o código tiver 7 dígitos, remova o último
Municipios$CODMUNRES_6dig <- ifelse(
  nchar(Municipios$Codigo_IBGE) == 7,
  substr(Municipios$Codigo_IBGE, 1, 6),
  Municipios$Codigo_IBGE # Mantém o original se não tiver 7 dígitos (ex: já tiver 6 ou menos)
)

# Criar uma versão de 6 dígitos do CD_GEOCMU no shapefile para junção
# Isso é CRUCIAL se seus dados `Municipios` tiverem códigos de 6 dígitos
shpMun$CD_GEOCMU_6dig <- substr(shpMun$CD_GEOCMU, 1, 6)
shpMun$CD_GEOCMU_6dig <- as.character(shpMun$CD_GEOCMU_6dig) # Garantir que é character

# --- 4. Juntar os dados da mortalidade ao shapefile ---
# Use left_join para manter todos os polígonos do shapefile
# Juntamos pelo código de 6 dígitos que criamos
shpMun_dados <- left_join(shpMun, Municipios, by = c("CD_GEOCMU_6dig" = "CODMUNRES_6dig"))

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
  )
)

# --- 5. Plotar com `geom_sf()` ---
ggplot(shpMun_dados) +
  # Camada dos municípios com preenchimento pela taxa de mortalidade
  geom_sf(
    aes(fill = media_temperatura_maxima),
    color = NA,
    size = 0.01
  ) +
  # Camada das fronteiras dos estados (diretamente do shpUFs)
  geom_sf(
    data = shpUFs,
    color = 'white',
    size = 0.01,
    fill = NA
  ) +
  # Escala de cores
  scale_fill_gradient2(
    low = 'lightblue',
    mid = '#FFDE00',
    high = '#FF0000',
    label = scales::comma_format(accuracy = .1),
    name = "Max. Average Temp. (°C)",
    na.value = "gray90" # This fills NA values with gray90
  ) +
  geom_text(data = capitals_data,
            aes(x = long, y = lat, label = capital),
            vjust = 0, hjust = 0.5, size = 3, check_overlap = TRUE,
            nudge_y = -1.2,
            color = "black") +
  theme_void() +
  ggtitle("") +
  theme(
    legend.position = c(0.1, 0.3),
    plot.background = element_rect(fill = "white", color = NA) # Example for plot background
  )
