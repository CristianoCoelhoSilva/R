library(readr)
library(dplyr)
library(lubridate)
library(readxl) # Necessário para read_excel se a função anterior não for chamada
library(tidyr)  # Necessário para pivot_longer/wider se a função anterior não for chamada
library(writexl) # Necessário para write_xlsx
library(readr)
library(dplyr)
library(purrr) # Para a função map_df

pasta_de_arquivos <- 'TEMPERATURA/TEMPERATURA/AUTOMATICAS'

#source('TEMPERATURA/TEMPERATURA_CODES/Normais_estacoes.R')
#normal_pivot <- processa_dados_temperatura(base_path = '')

# --- 3. Listar todos os arquivos na pasta ---
# Use o padrão "*.csv" se você tiver outros tipos de arquivos na pasta e quiser apenas CSVs.
# full.names = TRUE garante que o caminho completo do arquivo seja retornado.
todos_os_arquivos <- list.files(path = pasta_de_arquivos, pattern = "\\.csv$", full.names = TRUE)

temperatura <- map_df(todos_os_arquivos, read_csv)

temperatura <- temperatura %>%
  mutate(
    DATA = ymd(DATA), # Garante que DATA é um objeto de data
    Mes = format(DATA, "%B"), # %B retorna o nome completo do mês
    Ano = format(DATA, "%Y") # %Y retorna o ano completo
  )
  
  # Reordena as colunas
  temperatura <- temperatura[c(1, 6, 7, 4, 5, 2, 3)]
  
  # Converte o nome do mês para maiúsculas
  temperatura$Mes <- toupper(temperatura$Mes)
  
  # Carrega o catálogo de estações automáticas
  estacoesAuto <- read.delim(paste0("TEMPERATURA/ESTACOES/CatalogoEstaçõesAutomáticas.csv"),
                             sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")
  
  estacoesAuto$OLD_NAME <- estacoesAuto$DC_NOME
  estacoesAuto$DC_NOME <- gsub("SAO PAULO\\(MIR,de SANTANA\\)|SAO PAULO - MIRANTE", "SAO PAULO", estacoesAuto$DC_NOME)
  estacoesAuto$DC_NOME <- gsub("SAO GABRIEL DA CACHOEIRA\\(UAUPES\\)", "SAO GABRIEL DA CACHOEIRA", estacoesAuto$DC_NOME)
  estacoesAuto$DC_NOME <- gsub("SERIDO \\(CAICO\\)", "CAICO", estacoesAuto$DC_NOME)
  estacoesAuto$DC_NOME <- gsub("SALVADOR \\(ONDINA\\)", "SALVADOR", estacoesAuto$DC_NOME)
  estacoesAuto$DC_NOME <- gsub("BOM JESUS DO PIAUI", "BOM JESUS", estacoesAuto$DC_NOME)
  estacoesAuto$DC_NOME <- gsub("RIO DE JANEIRO - FORTE DE COPACABANA|RIO DE JANEIRO - JACAREPAGUA|RIO DE JANEIRO - VILA MILITAR|RIO DE JANEIRO-MARAMBAIA", "RIO DE JANEIRO", estacoesAuto$DC_NOME)
  estacoesAuto$DC_NOME <- gsub("AGUAS EMENDADAS|BRAZLANDIA|GAMA \\(PONTE ALTA\\)|PARANOA \\(COOPA-DF\\)", "BRASILIA", estacoesAuto$DC_NOME)
  estacoesAuto$DC_NOME <- gsub("BELO HORIZONTE - PAMPULHA|BELO HORIZONTE - CERCADINHO", "BELO HORIZONTE", estacoesAuto$DC_NOME)
  estacoesAuto$DC_NOME <- gsub("PORTO ALEGRE - JARDIM BOTANICO", "PORTO ALEGRE", estacoesAuto$DC_NOME)
  
  # Junta os dados de temperatura com as informações das estações automáticas
  temperatura <- temperatura %>%
    inner_join(estacoesAuto, by = c("CODIGO_ESTACAO" = "CD_ESTACAO"))
  
  # Reordena as colunas após a junção
  temperatura <- temperatura[c(1, 2, 3, 8, 4,5, 6)]
  
  # Substitui valores infinitos por NA nas colunas de temperatura
  temperatura$temperatura_maxima[is.infinite(temperatura$temperatura_maxima)] <- NA
  #temperatura$temperatura_minima[is.infinite(temperatura$temperatura_minima)] <- NA
  
  temperatura$temperatura_maxima[temperatura$temperatura_maxima == -9999] <- NA
  #temperatura$temperatura_minima[temperatura$temperatura_minima == -9999] <- NA
  
  # Remove linhas onde tanto a temperatura máxima quanto a mínima são NA
  # temperatura <- temperatura %>% filter(!is.na(temperatura_maxima) | !is.na(temperatura_minima))
  
  temperatura <- temperatura %>% filter(!is.na(temperatura_maxima))
  
  # Junta os dados de temperatura com as normais climatológicas (normal_pivot)
  # temperatura <- temperatura %>% inner_join(normal_pivot, by = c("CODIGO_ESTACAO" = "CD_ESTACAO", "Mes" = "Mes"))
  
  # Reordena as colunas finais
  # temperatura <- temperatura[c(1,2,3,4,5,11,6,7,8,12, 13)]
  
  # Substitui vírgulas por pontos e converte para numérico as colunas de temperatura
  # (Isso é feito novamente aqui, caso os dados de entrada não tenham sido limpos previamente)
  temperatura$temperatura_maxima <- as.numeric(gsub(",", ".", temperatura$temperatura_maxima))
  #temperatura$temperatura_minima <- as.numeric(gsub(",", ".", temperatura$temperatura_minima))
  ##temperatura$Max <- as.numeric(gsub(",", ".", temperatura$Max))
  ##temperatura$Min <- as.numeric(gsub(",", ".", temperatura$Min))
  
  temperatura <- temperatura %>%
    group_by(DATA, Mes, Ano, DC_NOME, ESTADO) %>%
    summarise(
      temperatura_maxima = mean(temperatura_maxima, na.rm = TRUE),
      .groups = 'drop' # Corrigido: vírgula antes e dentro da função summarise
    )
  
  
  # Obter códigos IBGE
  dados_cidades <- temperatura %>%
    select(ESTADO, DC_NOME) %>%
    unique() %>%
    mutate(Codigo_IBGE = NA_character_) # Usar NA_character_ para garantir que seja string
  
  municipios_br <- read_excel("TEMPERATURA/MUNICIPIOS/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.xls") %>%
    mutate(name_muni = iconv(name_muni, from = "UTF-8", to = "ASCII//TRANSLIT"),
           name_muni = toupper(name_muni))
  
  map_uf_sigla <- c(
    "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA",
    "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE",
    "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE",
    "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
    "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT",
    "52" = "GO", "53" = "DF"
  )
  
  municipios_br$sigla_uf <- map_uf_sigla[as.character(municipios_br$UF)]
  
  for (i in 1:nrow(dados_cidades)) {
    estado_atual <- dados_cidades$ESTADO[i]
    cidade_atual <- dados_cidades$DC_NOME[i]
    
    match_ibge <- municipios_br %>%
      filter(sigla_uf == estado_atual, name_muni == cidade_atual)
    
    if (nrow(match_ibge) > 0) {
      dados_cidades$Codigo_IBGE[i] <- as.character(match_ibge$codigo[1]) # Converter para string
    }
  }
  
  dados_cidades <- dados_cidades %>% filter(!is.na(Codigo_IBGE))
  
  temperatura <- temperatura %>%
    inner_join(dados_cidades, by = c("ESTADO" = "ESTADO", "DC_NOME" = "DC_NOME"))

  temperatura$Codigo_IBGE <- as.numeric(temperatura$Codigo_IBGE)
  temperatura$Codigo_IBGE <- substr(temperatura$Codigo_IBGE, 1, 6)
  temperatura$Codigo_IBGE <- as.numeric(temperatura$Codigo_IBGE)
  codigos_ibge_capitais <- c(110020, 120040, 130260, 140010, 150140, 160030, 172100, 211130, 221100, 230440, 240810, 250750, 261160, 270430, 280030, 292740, 310620, 320530, 330455, 355030, 410690, 420540, 431490, 500270, 510340, 520870, 530010)
  temperatura <- temperatura[temperatura$Codigo_IBGE %in% codigos_ibge_capitais, ]

  Normal_TMAX <- read_excel("TEMPERATURA/NORMAIS/Normal-Climatologica-TMAX.xlsx")
  Normal_TMAX <- Normal_TMAX[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
  
  Normal_TMAX <- Normal_TMAX %>%
    mutate(across(Janeiro:Dezembro, ~ as.numeric(gsub(",", ".", .)))) %>%
    pivot_longer(
      cols = Janeiro:Dezembro,
      names_to = "Mes",
      values_to = "Temperatura_Normal"
    ) %>%
    arrange(`Nome da Estação`, UF)
  
  Normal_TMAX$Mes <- toupper(Normal_TMAX$Mes)
  
  temperatura <- inner_join(temperatura, Normal_TMAX, by = c("ESTADO" = "UF", "DC_NOME" = "Nome da Estação", 'Mes' = 'Mes'))
  
  #write.csv(temperatura, "my_data.csv", row.names = FALSE)
  temperatura$temperatura_quadrado <- temperatura$temperatura_maxima^2  
  
  temperatura <- temperatura %>%
    mutate(Y34 = ifelse(temperatura_maxima >= 34 & temperatura_maxima < 36, TRUE, FALSE))
  
  #rm(list = setdiff(ls(), "temperatura"))
  temperatura <- temperatura %>%
    mutate(Y36 = ifelse(temperatura_maxima >= 36 & temperatura_maxima < 38, TRUE, FALSE))
  
  temperatura <- temperatura %>%
    mutate(Y38 = ifelse(temperatura_maxima >= 38 & temperatura_maxima < 40, TRUE, FALSE))
  
  temperatura <- temperatura %>%
    mutate(Y40 = ifelse(temperatura_maxima >= 40 , TRUE, FALSE))
  
  temperatura <- temperatura %>%
    arrange(DC_NOME, DATA)
  
  temperatura <- temperatura %>%
    group_by(DC_NOME) %>% 
    mutate(temperatura_media_3dias = zoo::rollmeanr(temperatura_maxima, k = 3, fill = NA, align = "right")) %>%
    ungroup()
  
  temperatura <- temperatura %>%
    group_by(DC_NOME) %>% 
    mutate(temperatura_media_30dias = zoo::rollmeanr(temperatura_maxima, k = 30, fill = NA, align = "right")) %>%
    ungroup()
  
  temperatura <- temperatura
  
  temperatura$EHISIG <- temperatura2$temperatura_media_3dias - temperatura2$Temperatura_Normal
  temperatura$EHIACCL <- temperatura2$temperatura_media_3dias - temperatura2$temperatura_media_30dias
  temperatura$EHF <- temperatura2$EHISIG * temperatura2$EHIACCL

  rm(list = setdiff(ls(), "temperatura"))
  
  library(xlsx)
  write.xlsx(temperatura, "dados_temperatura.xlsx")
  