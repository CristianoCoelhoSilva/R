library(readr)
library(dplyr)

# Ler o arquivo CSV com read_csv2 (para arquivos separados por ponto e vírgula)
enem <- read_csv2(
  "~/AULA/ENEM/DADOS/MICRODADOS_ENEM_2023.csv",
  col_select = c(
    NO_MUNICIPIO_PROVA,
    CO_UF_ESC,
    TP_DEPENDENCIA_ADM_ESC,
    Q006,
    NU_NOTA_MT,
    NU_NOTA_LC,
    NU_NOTA_REDACAO,
    TP_COR_RACA,
    TP_SEXO
  ),
  col_types = cols(
    NU_NOTA_MT = col_character(),
    NU_NOTA_LC = col_character()
  ),
  locale = locale(
    decimal_mark = ",",
    grouping_mark = ".",
    encoding = "latin1" # Especifica a codificação dentro de locale()
  )
)

enem <- enem[complete.cases(enem), ]

enem <- enem %>%
  mutate(
    Classe_Renda = case_when(
      Q006 %in% c("K", "L", "M", "N", "O", "P", "Q") ~ "Rico",
      Q006 %in% c("A", "B", "C", "D") ~ "Pobre",
      TRUE ~ "Média"
    )
  )

enem <- enem %>%
  mutate(
    Raca = case_when(
      TP_COR_RACA == 1 ~ "Branca",
      TP_COR_RACA == 2 ~ "Não Branca",
      TP_COR_RACA == 3 ~ "Não Branca",
      TP_COR_RACA == 4 ~ "Amarela",
      TP_COR_RACA == 5 ~ "Indígena",
      TP_COR_RACA == 6 ~ "Não dispõe da informação",
      TRUE ~ as.character(TP_COR_RACA)
    )
  )

enem <- enem %>%
  rename(Genero = TP_SEXO)

enem <- enem %>%
  mutate(
    Classe_Renda = case_when(
      Q006 %in% c("K", "L", "M", "N", "O", "P", "Q") ~ "Rico",
      Q006 %in% c("A", "B", "C", "D") ~ "Pobre",
      TRUE ~ "Média"
    )
  )

# Filtrando o dataframe para incluir apenas as categorias "Branca" e "Não Branca" na coluna "Raca"
enem <- enem %>%
  filter(Raca %in% c("Branca", "Não Branca"))

enem <- enem %>%   mutate(     Renda_Familiar = recode(       Q006,       "A" = "Nenhuma Renda",       "B" = "Até R$ 1.320,00",       "C" = "De R$ 1.320,01 até R$ 1.980,00",       "D" = "De R$ 1.980,01 até R$ 2.640,00",       "E" = "De R$ 2.640,01 até R$ 3.300,00",       "F" = "De R$ 3.300,01 até R$ 3.960,00",       "G" = "De R$ 3.960,01 até R$ 5.280,00",       "H" = "De R$ 5.280,01 até R$ 6.600,00",       "I" = "De R$ 6.600,01 até R$ 7.920,00",       "J" = "De R$ 7.920,01 até R$ 9240,00",       "K" = "De R$ 9.240,01 até R$ 10.560,00",       "L" = "De R$ 10.560,01 até R$ 11.880,00",       "M" = "De R$ 11.880,01 até R$ 13.200,00",       "N" = "De R$ 13.200,01 até R$ 15.840,00",       "O" = "De R$ 15.840,01 até R$19.800,00",       "P" = "De R$ 19.800,01 até R$ 26.400,00",       "Q" = "Acima de R$ 26.400,00"     )   )