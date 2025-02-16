# Defina os caminhos dos arquivos CSV
arquivo1 <- "DADOS/Bitcoin1.csv"
arquivo2 <- "DADOS/Bitcoin2.csv"
arquivo3 <- "DADOS/Bitcoin3.csv"
arquivo4 <- "DADOS/Bitcoin4.csv"
arquivo5 <- "DADOS/Bitcoin5.csv"
arquivo6 <- "DADOS/Bitcoin6.csv"

# Leia os arquivos CSV
df1 <- read.csv(arquivo1, stringsAsFactors = FALSE)
df2 <- read.csv(arquivo2, stringsAsFactors = FALSE)
df3 <- read.csv(arquivo3, stringsAsFactors = FALSE)
df4 <- read.csv(arquivo4, stringsAsFactors = FALSE)
df5 <- read.csv(arquivo5, stringsAsFactors = FALSE)
df6 <- read.csv(arquivo6, stringsAsFactors = FALSE)

# Combine os dataframes
# Se as colunas forem iguais, use rbind
# Se forem diferentes, use merge ou dplyr::bind_rows
df_combinado <- rbind(df1, df2, df3, df4, df5, df6)

# Escreva o resultado em um novo arquivo CSV
write.csv(df_combinado, "DADOS/BTC_EUA.csv", row.names = FALSE)

Bitcoin <- read.csv('DADOS/BTC_EUA.csv', stringsAsFactors = FALSE)

Bitcoin$Data <- as.Date(Bitcoin$Data, format = "%d.%m.%Y")

# Definir o intervalo de datas desejado
data_inicio <- min(Bitcoin$Data)
data_fim <- max(Bitcoin$Data)

# Criar uma sequência completa de datas
data_completa <- seq(data_inicio, data_fim, by = "day")

# Verificar quais datas estão faltando
datas_faltantes <- setdiff(data_completa, Bitcoin$Data)

# Exibir as datas ausentes
print(datas_faltantes)

