normais_estacoes <- function (Pivot) {
  
library(readxl)

normais <- read_excel("TEMPERATURA/DADOS/normais climatologicas.xlsx")

if (Pivot) {

# Primeiro, vamos criar um identificador para cada combinação de Cidade e Mês
normais_long <- normais %>%
  pivot_longer(
    cols = c(MINIMA, MAXIMA),
    names_to = "Tipo",
    values_to = "Valor"
  ) %>%
  mutate(
    Tipo_Mes = paste(Tipo, Mês, sep = "_")
  )

# Agora, vamos fazer o pivot para ter os meses como colunas
dados_final <- normais_long %>%
  pivot_wider(
    id_cols = Cidade,
    names_from = Tipo_Mes,
    values_from = Valor
  )

# Para deixar no formato exatamente como solicitado (coluna "Tipo" e meses)
# vamos fazer mais uma transformação

normais <- normais %>%
  pivot_longer(
    cols = c(MINIMA, MAXIMA),
    names_to = "Tipo",
    values_to = "Valor"
  ) %>%
  pivot_wider(
    id_cols = c(Cidade, Tipo),
    names_from = Mês,
    values_from = Valor
  )


}

estacoes <- read_excel("TEMPERATURA/DADOS/estacoes.xlsx")

normais_estacoes <- inner_join(estacoes, normais, by = c("estacao" = "Cidade"), relationship = "many-to-many")

return(normais_estacoes)

}