library(readr)
library(readxl)
#library(sqldf)
library(dplyr)
library(writexl)
library(geobr)
library(sf)     # Carregue o pacote sf explicitamente
library(dplyr)
library(tidyr)
library(dplyr)

estacoesConv <-  read.delim("TEMPERATURA/ESTACOES/CatalogoEstaçõesConvencionais.csv", sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")

estacoesAuto <-  read.delim("TEMPERATURA/ESTACOES/CatalogoEstaçõesAutomáticas.csv", sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")

Normal_TMAX <- read_excel("TEMPERATURA/NORMAIS/Normal-Climatologica-TMAX.xlsx")

Base_Conv_MAX <- inner_join(estacoesConv, Normal_TMAX, by = c("CD_ESTACAO" = "Código"))

Base_Auto_MAX <- inner_join(estacoesAuto, Normal_TMAX, by = c("SG_ESTADO" = "UF", "DC_NOME" = "Nome da Estação"))

rm(Normal_TMAX)

Base_Auto_MAX <- Base_Auto_MAX[c(2, 1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)]

Base_Conv_MAX <- Base_Conv_MAX[c(2, 1, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)]

normal_max <- rbind(Base_Auto_MAX, Base_Conv_MAX)

normal_max <- unique(normal_max)

Normal_TMIN <- read_excel("TEMPERATURA/NORMAIS/Normal-Climatologica-TMIN.xlsx")

Base_Conv_TMIN <- inner_join(estacoesConv, Normal_TMIN, by = c("CD_ESTACAO" = "Código"))

Base_Auto_TMIN <- inner_join(estacoesAuto, Normal_TMIN, by = c("SG_ESTADO" = "UF", "DC_NOME" = "Nome da Estação"))

rm(Normal_TMIN)

Base_Auto_TMIN <- Base_Auto_TMIN[c(2, 1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)]

Base_Conv_TMIN <- Base_Conv_TMIN[c(2, 1, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)]

normal_min <- rbind(Base_Auto_TMIN, Base_Conv_TMIN)

normal_min <- unique(normal_min)

normal_max$TIPO <- 'Max'
normal_min$TIPO <- 'Min'

normal <- rbind(normal_max, normal_min)

rm(list = setdiff(ls(), c("normal")))



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

normal <- normal %>%
  left_join(dados_cidades, by = c("SG_ESTADO" = "SG_ESTADO", "DC_NOME" = "DC_NOME"))

normal <- normal[c(1, 2, 16, 15, 3, 4, 5, 6 ,7 ,8 ,9 , 10, 11, 12, 13, 14)]

rm(list = setdiff(ls(), c("normal")))

normal_pivot <- normal %>%
  mutate(across(Janeiro:Dezembro, ~ as.numeric(gsub(",", ".", .)))) %>%
  # 2. Pivotar as colunas de mês para linhas
  pivot_longer(
    cols = Janeiro:Dezembro, # As colunas que queremos pivotar
    names_to = "Mes",        # Nome da nova coluna que armazenará os nomes dos meses
    values_to = "Temperatura" # Nome da nova coluna que armazenará os valores de temperatura
  ) %>%
  # 3. Criar as colunas 'Max' e 'Min' a partir da coluna 'TIPO' e 'Temperatura'
  pivot_wider(
    names_from = TIPO,     # A coluna 'TIPO' vai virar os nomes das novas colunas
    values_from = Temperatura # Os valores da coluna 'Temperatura' vão preencher as novas colunas
  ) %>%
  # Opcional: Reordenar as colunas para melhor visualização
  select(SG_ESTADO, DC_NOME, Codigo_IBGE, Mes, Max, Min) %>%
  # Opcional: Organizar por estado, cidade e mês
  arrange(SG_ESTADO, DC_NOME, Mes)

normal_pivot$Mes <- toupper(normal_pivot$Mes)

write_xlsx(normal, path = "TEMPERATURA/NORMAIS/Normais.xlsx")
write_xlsx(normal, path = "TEMPERATURA/NORMAIS/Normais_pivot.xlsx")
