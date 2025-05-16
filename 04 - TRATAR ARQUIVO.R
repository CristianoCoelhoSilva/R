library(readr)

# Defina o ano desejado aqui
ano_desejado <- 2013

# Crie o nome do arquivo de entrada usando a variável ano_desejado
nome_arquivo_entrada <- paste0("~/PROJETO/DADOS/COMBINADOS_", ano_desejado, ".csv")

# Carregue os dados
base <- read_csv(nome_arquivo_entrada)

# Selecione as colunas
base <- base[, c("ORIGEM", "CODMUNRES", "CODMUNRES", "IDADE", "def_sexo", "def_raca_cor", "def_escol", "causabas_capitulo", "causabas_categoria", "causabas_grupo", "causabas_subcategoria", "idade_obito", "ano_obito", "DTOBITO")]

# Crie o nome do arquivo de saída usando a variável ano_desejado
nome_arquivo_saida <- paste0("C:/Users/cristiano.silva/Documents/PROJETO/DADOS/FINAL", ano_desejado, ".csv")

# Salve o arquivo
write.csv(base, file = nome_arquivo_saida, row.names = FALSE, quote = TRUE, na = "")
