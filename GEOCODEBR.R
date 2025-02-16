library(geocodebr)

input_df <- read.csv(system.file("extdata/small_sample.csv", package = "geocodebr"))

# Primeiro passo: inidicar o nome das colunas com cada campo dos enderecos
campos <- geocodebr::definir_campos(
  logradouro = "nm_logradouro",
  numero = "Numero",
  cep = "Cep",
  localidade = "Bairro",
  municipio = "nm_municipio",
  estado = "nm_uf"
)

df <- geocodebr::geocode(
  enderecos = input_df,
  campos_endereco = campos,
  resultado_completo = FALSE,
  resolver_empates = TRUE,
  resultado_sf = FALSE,
  verboso = FALSE,
  cache = TRUE,
  n_cores = 1
)
