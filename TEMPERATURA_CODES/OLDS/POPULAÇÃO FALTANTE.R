library(geobr)
library(dplyr) 
library(foreign)

ibge <- read_csv("TEMPERATURA/MUNICIPIOS/ibge_2002_2019_comPopulacao.csv")

ibge <- ibge %>%
  filter(POPULACAO > 0 & PIB > 0) %>%
  mutate(IBGE = floor(IBGE / 10))

codigos_ibge_capitais <- c(110020, 120040, 130260, 140010, 150140, 160030, 172100, 211130, 221100, 230440, 240810, 250750, 261160, 270430, 280030, 292740, 310620, 320530, 330455, 355030, 410690, 420540, 431490, 500270, 510340, 520870, 530010)
ibge <- ibge[ibge$IBGE %in% codigos_ibge_capitais, ]

ibge <- ibge[c('ANO','REGIAO','UF','NOME','IBGE','POPULACAO')]


arquivo_dbf <- "C:/Users/cristiano.silva/Documents/PROJETOS/TEMPERATURA/POPSBR22/POP20.dbf"
B2020 <- read.dbf(arquivo_dbf)

arquivo_dbf <- "C:/Users/cristiano.silva/Documents/PROJETOS/TEMPERATURA/POPSBR22/POP21.dbf"
B2021 <- read.dbf(arquivo_dbf)

arquivo_dbf <- "C:/Users/cristiano.silva/Documents/PROJETOS/TEMPERATURA/POPSBR22/POP22.dbf"
B2022 <- read.dbf(arquivo_dbf)

arquivo_dbf <- "C:/Users/cristiano.silva/Documents/PROJETOS/TEMPERATURA/POPSBR22/POP23.dbf"
B2023 <- read.dbf(arquivo_dbf)


base <- rbind(B2020, B2021, B2022, B2023)

base <- base %>%
  mutate(
    COD_MUN = as.numeric(as.character(COD_MUN)),
    COD_MUN = floor(COD_MUN / 10))

codigos_ibge_capitais <- c(110020, 120040, 130260, 140010, 150140, 160030, 172100, 211130, 221100, 230440, 240810, 250750, 261160, 270430, 280030, 292740, 310620, 320530, 330455, 355030, 410690, 420540, 431490, 500270, 510340, 520870, 530010)
base <- base[base$COD_MUN %in% codigos_ibge_capitais, ]

base <- base %>%
  group_by(COD_MUN, ANO) %>%
  summarise(
    Total_POP = sum(POP, na.rm = TRUE) # Soma a população para cada grupo
  ) %>%
  ungroup()

dados_municipios_geobr <- read_municipality(year = 2020)

dados_municipios_geobr <- dados_municipios_geobr %>%
  mutate(
    code_muni = as.numeric(as.character(code_muni)),
    code_muni = floor(code_muni / 10))

base =
  base %>%
  inner_join(dados_municipios_geobr %>%
               select(COD_MUN = code_muni,
                      abbrev_state,
                      name_muni,
                      name_region
               ))

base <- base[c(2,6,4,5,1,3)]

base <- base %>%
  rename(
    ANO = ANO,           
    REGIAO = name_region,
    UF = abbrev_state,
    NOME = name_muni,
    IBGE = COD_MUN,    
    POPULACAO = Total_POP
  )

IBGEFINAL <- rbind(ibge, base)


write.csv(IBGEFINAL, "TEMPERATURA/MUNICIPIOS/ibge_2002_2023_comPopulacao.csv", row.names = FALSE)
