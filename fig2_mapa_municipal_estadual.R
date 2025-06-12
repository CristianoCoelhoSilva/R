library(sf)
library(dplyr)
library(ggplot2)
library(psych)
library(scales) 
library(stringr)

shpMun <- st_read("PROJETO/PROJETO/DADOS/IBGE/BRMUE250GC_SIR.shp")
shpUFs <- st_read("PROJETO/PROJETO/DADOS/IBGE/BRUFE250GC_SIR.shp")

Municipios <- tudo[c("CODMUNRES", "ano_obito", "taxa_mortalidade")]
Municipios <- Municipios[Municipios$ano_obito == 2002, ]

# --- 3. Processar `CODMUNRES` e `CD_GEOCMU` para junção ---
# Converte CODMUNRES para caractere (importante para manipulação de string)
Municipios$CODMUNRES <- as.character(Municipios$CODMUNRES)

# APlica a sua regra: se o código tiver 7 dígitos, remova o último
Municipios$CODMUNRES_6dig <- ifelse(
  nchar(Municipios$CODMUNRES) == 7,
  substr(Municipios$CODMUNRES, 1, 6),
  Municipios$CODMUNRES # Mantém o original se não tiver 7 dígitos (ex: já tiver 6 ou menos)
)

# Criar uma versão de 6 dígitos do CD_GEOCMU no shapefile para junção
# Isso é CRUCIAL se seus dados `Municipios` tiverem códigos de 6 dígitos
shpMun$CD_GEOCMU_6dig <- substr(shpMun$CD_GEOCMU, 1, 6)
shpMun$CD_GEOCMU_6dig <- as.character(shpMun$CD_GEOCMU_6dig) # Garantir que é character

# --- 4. Juntar os dados da mortalidade ao shapefile ---
# Use left_join para manter todos os polígonos do shapefile
# Juntamos pelo código de 6 dígitos que criamos
shpMun_dados <- left_join(shpMun, Municipios, by = c("CD_GEOCMU_6dig" = "CODMUNRES_6dig"))

# --- 5. Plotar com `geom_sf()` ---
ggplot(shpMun_dados) +
  # Camada dos municípios com preenchimento pela taxa de mortalidade
  geom_sf(
    aes(fill = taxa_mortalidade),
    color = NA, # Changed from 'black' to NA
    size = 0.01
  ) +
  # Camada das fronteiras dos estados (diretamente do shpUFs)
  geom_sf(
    data = shpUFs,
    color = NA, # Changed from 'black' to NA
    size = 0.01,
    fill = NA
  ) +
  # Escala de cores
  scale_fill_gradient(
    low = 'lightyellow',
    high = 'darkred',
    label = scales::comma_format(accuracy = .1),
    name = "Rate"
  ) +
  # Tema e títulos
  theme_void() +
  ggtitle("") +
  theme(legend.position = c(0.1, 0.3))
