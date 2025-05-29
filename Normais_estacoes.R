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

estacoesConv$OLD_NAME <- estacoesConv$DC_NOME

estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "SAO PAULO(MIR,de SANTANA)", "SAO PAULO", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "SAO GABRIEL DA CACHOEIRA(UAUPES)", "SAO GABRIEL DA CACHOEIRA", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "SERIDO (CAICO)", "CAICO", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "SALVADOR (ONDINA)", "SALVADOR", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "BOM JESUS DO PIAUI", "BOM JESUS", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "RIO DE JANEIRO - FORTE DE COPACABANA", "RIO DE JANEIRO", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "RIO DE JANEIRO - JACAREPAGUA", "RIO DE JANEIRO", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "RIO DE JANEIRO - VILA MILITAR", "RIO DE JANEIRO", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "RIO DE JANEIRO-MARAMBAIA", "RIO DE JANEIRO", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "AGUAS EMENDADAS", "BRASILIA", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "BRAZLANDIA", "BRASILIA", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "GAMA (PONTE ALTA)", "BRASILIA", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "PARANOA (COOPA-DF)", "BRASILIA", estacoesConv$DC_NOME)
estacoesConv$DC_NOME <- ifelse(estacoesConv$DC_NOME == "SAO PAULO - MIRANTE", "SAO PAULO", estacoesConv$DC_NOME)

estacoesAuto <-  read.delim("TEMPERATURA/ESTACOES/CatalogoEstaçõesAutomáticas.csv", sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")

estacoesAuto$OLD_NAME <- estacoesAuto$DC_NOME

estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "SAO PAULO(MIR,de SANTANA)", "SAO PAULO", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "SAO GABRIEL DA CACHOEIRA(UAUPES)", "SAO GABRIEL DA CACHOEIRA", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "SERIDO (CAICO)", "CAICO", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "SALVADOR (ONDINA)", "SALVADOR", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "BOM JESUS DO PIAUI", "BOM JESUS", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "RIO DE JANEIRO - FORTE DE COPACABANA", "RIO DE JANEIRO", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "RIO DE JANEIRO - JACAREPAGUA", "RIO DE JANEIRO", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "RIO DE JANEIRO - VILA MILITAR", "RIO DE JANEIRO", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "RIO DE JANEIRO-MARAMBAIA", "RIO DE JANEIRO", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "AGUAS EMENDADAS", "BRASILIA", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "BRAZLANDIA", "BRASILIA", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "GAMA (PONTE ALTA)", "BRASILIA", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "PARANOA (COOPA-DF)", "BRASILIA", estacoesAuto$DC_NOME)
estacoesAuto$DC_NOME <- ifelse(estacoesAuto$DC_NOME == "SAO PAULO - MIRANTE", "SAO PAULO", estacoesAuto$DC_NOME)

Normal_TMAX <- read_excel("TEMPERATURA/NORMAIS/Normal-Climatologica-TMAX.xlsx")

Base_Conv_MAX <- inner_join(estacoesConv, Normal_TMAX, by = c("CD_ESTACAO" = "Código"))

Base_Auto_MAX <- inner_join(estacoesAuto, Normal_TMAX, by = c("SG_ESTADO" = "UF", "DC_NOME" = "Nome da Estação"))

Base_Auto_MAX <- Base_Auto_MAX[c(2, 1, 8, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)]

Base_Conv_MAX <- Base_Conv_MAX[c(2, 1, 8, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)]

normal_max <- rbind(Base_Auto_MAX, Base_Conv_MAX)

rm(list = setdiff(ls(), c("normal_max","estacoesAuto","estacoesConv")))

#normal_max <- unique(normal_max)

Normal_TMIN <- read_excel("TEMPERATURA/NORMAIS/Normal-Climatologica-TMIN.xlsx")

Base_Conv_TMIN <- inner_join(estacoesConv, Normal_TMIN, by = c("CD_ESTACAO" = "Código"))

Base_Auto_TMIN <- inner_join(estacoesAuto, Normal_TMIN, by = c("SG_ESTADO" = "UF", "DC_NOME" = "Nome da Estação"))

Base_Auto_TMIN <- Base_Auto_TMIN[c(2, 1, 8, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)]

Base_Conv_TMIN <- Base_Conv_TMIN[c(2, 1, 8, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)]

normal_min <- rbind(Base_Auto_TMIN, Base_Conv_TMIN)

#normal_min <- unique(normal_min)

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
  cidade_atual <- dados_cidades$DC_NOME[i]

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

normal <- normal[c(1, 2, 3, 16, 17, 4, 5, 6 ,7 ,8 ,9 , 10, 11, 12, 13, 14, 15)]

normal_pivot <- normal %>%
  mutate(across(Janeiro:Dezembro, ~ as.numeric(gsub(",", ".", .)))) %>%
  pivot_longer(
    cols = Janeiro:Dezembro,
    names_to = "Mes",
    values_to = "Temperatura"
  ) %>%
  pivot_wider(
    names_from = TIPO,
    values_from = Temperatura
  ) %>%
  select(SG_ESTADO, DC_NOME, CD_ESTACAO,Codigo_IBGE, Mes, Max, Min) %>%
  arrange(SG_ESTADO, DC_NOME, Mes)

normal_pivot$Mes <- toupper(normal_pivot$Mes)

rm(list = setdiff(ls(), c("normal_pivot")))

#write_xlsx(normal, path = "TEMPERATURA/NORMAIS/Normais.xlsx")
#write_xlsx(normal, path = "TEMPERATURA/NORMAIS/Normais_pivot.xlsx")
