# Carregar os pacotes
library(pof2017package)
library(dplyr)
library(survey)
library(openxlsx)
library(stringr)
library(readxl)

tradutor_despesa <- read_excel("Tradutor_Despesa_Geral.xls")
tradutor_despesa <- tradutor_despesa[tradutor_despesa$Variavel == 'V8000_DEFLA',]

dados_geral<- ler_pof_geral (arquivo_microdados = "DESPESA_COLETIVA.txt")

dados_despesas <- ler_pof_despesa_codigo (arquivo_microdados = "DESPESA_COLETIVA.txt")


dados_geral <- dados_geral %>%
  mutate(
    NUM_DOM = as.numeric(NUM_DOM)
  )

dados_despesas <- dados_despesas %>%
  mutate(
    NUM_DOM = as.numeric(NUM_DOM)
  )

dados <- dados_geral %>%
  select(COD_UPA, NUM_DOM, UF, ESTRATO_POF, PESO_FINAL)

dados <- distinct(dados)

despesas <- dados_despesas %>%
  left_join(dados, by = c("UF", "COD_UPA", "NUM_DOM"))

despesas <- despesas[c('UF','COD_UPA','NUM_DOM','valor', 'Descricao_3')]


total_valor <- despesas %>%
  group_by(UF, COD_UPA, NUM_DOM) %>%
  summarise(
    total_valor = sum(valor, na.rm = TRUE),
    total_itens = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_valor))

media_por_uf <- total_valor %>%
  group_by(UF) %>%
  summarise(media_total_valor = mean(total_valor),
            total_itens = n())
