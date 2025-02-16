rm(list = ls())
options(scipen = 999)
library(readxl)
library(writexl)

# Defina o caminho do arquivo Excel
arquivo_excel <- "DADOS/TRUMP1.xlsx"

# Leia a primeira planilha do arquivo
df <- read_excel(arquivo_excel, sheet = 1)


df$timeClose <- as.POSIXct(df$timeClose / 1000, origin = "1970-01-01", tz = "GMT")
df$timeOpen <- as.POSIXct(df$timeOpen / 1000, origin = "1970-01-01", tz = "GMT")
df$timeHigh <- as.POSIXct(df$timeHigh / 1000, origin = "1970-01-01", tz = "GMT")
df$timeLow <- as.POSIXct(df$timeLow / 1000, origin = "1970-01-01", tz = "GMT")

# Salve os dados em CSV
write.csv(df, 'DADOS/TRUMP.csv', row.names = FALSE, fileEncoding = "UTF-8")