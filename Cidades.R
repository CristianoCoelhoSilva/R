library(geobr)
library(sf)     # Carregue o pacote sf explicitamente
library(dplyr)

dados_cidades <- normal %>%
  select(SG_ESTADO, DC_NOME)

dados_cidades <- unique(dados_cidades)

dados_cidades$Codigo_IBGE <- NA

municipios_br <- read_municipality(year = 2020) %>%
  st_drop_geometry() %>%
  select(abbrev_state, name_muni, code_muni) %>% 
  mutate(name_muni = toupper(name_muni))

municipios_br$name_muni <- iconv(municipios_br$name_muni, from = "UTF-8", to = "ASCII//TRANSLIT")

for (i in 1:nrow(dados_cidades)) {
  estado_atual <- dados_cidades$SG_ESTADO[i]
  cidade_atual <- toupper(dados_cidades$DC_NOME[i]) # Converter para maiúsculas para comparação
  
  if (cidade_atual == "SAO GABRIEL DA CACHOEIRA(UAUPES)") cidade_atual <- "SAO GABRIEL DA CACHOEIRA"
  if (cidade_atual == "SERIDO (CAICO)") cidade_atual <- "CAICO"
  if (cidade_atual == "SALVADOR (ONDINA)") cidade_atual <- "SALVADOR"
  if (cidade_atual == "SAO PAULO(MIR,de SANTANA)") cidade_atual <- "SAO PAULO"
  if (cidade_atual == "BOM JESUS DO PIAUI") cidade_atual <- "BOM JESUS"
  
  # Tentar encontrar o código IBGE
  match_ibge <- municipios_br %>%
    filter(abbrev_state == estado_atual, name_muni == cidade_atual)
  
  if (nrow(match_ibge) > 0) {
    dados_cidades$Codigo_IBGE[i] <- match_ibge$code_muni[1]
  } else {
    # Se não encontrar, pode tentar uma busca mais flexível ou deixar NA
    # print(paste("Não encontrado:", cidade_atual, estado_atual)) # Para depuração
  }
}

normal$DC_NOME <- iconv(normal$DC_NOME, from = "UTF-8", to = "ASCII//TRANSLIT")

normal <- cBind(normal, dados_cidades, by = 'SG_ESTATE, DC_NAME')