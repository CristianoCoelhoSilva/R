# Defina os caminhos dos arquivos CSV
arquivo1 <- "DADOS/Bitcoin1.csv"
arquivo2 <- "DADOS/Bitcoin2.csv"
arquivo3 <- "DADOS/Bitcoin3.csv"
arquivo4 <- "DADOS/Bitcoin4.csv"
arquivo5 <- "DADOS/Bitcoin5.csv"


# Leia os arquivos CSV
df1 <- read.csv(arquivo1, stringsAsFactors = FALSE)
df2 <- read.csv(arquivo2, stringsAsFactors = FALSE)
df3 <- read.csv(arquivo3, stringsAsFactors = FALSE)
df4 <- read.csv(arquivo4, stringsAsFactors = FALSE)
df5 <- read.csv(arquivo5, stringsAsFactors = FALSE)

# Combine os dataframes
# Se as colunas forem iguais, use rbind
# Se forem diferentes, use merge ou dplyr::bind_rows
df_combinado <- rbind(df1, df2, df3, df4, df5)

# Escreva o resultado em um novo arquivo CSV
write.csv(df_combinado, "DADOS/Bitcoin.csv", row.names = FALSE)
