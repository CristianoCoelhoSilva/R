# Leia o arquivo
library(readr)

diretorio_origem <- "C:/Users/cristiano.silva/Documents/TEMPERATURA/2024/FAZER"
diretorio_destino <- "C:/Users/cristiano.silva/Documents/TEMPERATURA/2024/FEITOS"
diretorio <- "C:/Users/cristiano.silva/Documents/TEMPERATURA/2024/TRATADOS"

todos_arquivos <- list.files(diretorio_origem)
arquivos <- head(todos_arquivos, 600)
arquivos <- as.array(arquivos)

for (v in arquivos) {

nome_arquivo <- v

print(nome_arquivo)

caminho_arquivo_origem <- file.path(diretorio_origem, nome_arquivo)
caminho_arquivo_destino <- file.path(diretorio_destino, nome_arquivo)

arquivo = v

arquivo <- file.path(diretorio_origem, v)

data <- read.csv(arquivo, sep = ";", skip = 8, fileEncoding = "latin1")

data <- data[, c(1, 2, 10, 11)]

# Renomeia as colunas
colnames(data) <- c("Date", "hour", "max temperature", "min temperature")

data$`max temperature` <- gsub(",", ".", data$`max temperature`, fixed = TRUE)
data$`min temperature` <- gsub(",", ".", data$`min temperature`, fixed = TRUE)

nome_arquivo <- basename(arquivo)

nome_arquivo <- v

caminho_completo <- file.path(diretorio, nome_arquivo)

write.csv(data, file = caminho_completo, row.names = FALSE)

sucesso <- file.rename(from = caminho_arquivo_origem, to = caminho_arquivo_destino)

if (sucesso) {
  cat(paste("Arquivo", nome_arquivo, "movido com sucesso para", diretorio_destino, "/n"))
} else {
  cat(paste("Erro ao mover o arquivo", nome_arquivo, "/n"))
}

}

#Colunas
#1-Data;
#2-Hora UTC;
#3-PRECIPITAÇÃO TOTAL, HORÁRIO (mm);
#4PRESSAO ATMOSFERICA AO NIVEL DA ESTACAO, HORARIA (mB);
#5PRESSÃO ATMOSFERICA MAX.NA HORA ANT. (AUT) (mB);
#6PRESSÃO ATMOSFERICA MIN. NA HORA ANT. (AUT) (mB);
#7RADIACAO GLOBAL (Kj/m²);
#8TEMPERATURA DO AR - BULBO SECO, HORARIA (°C);
#9TEMPERATURA DO PONTO DE ORVALHO (°C);
#10TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C);
#11TEMPERATURA MÍNIMA NA HORA ANT. (AUT) (°C);
#TEMPERATURA ORVALHO MAX. NA HORA ANT. (AUT) (°C);
#TEMPERATURA ORVALHO MIN. NA HORA ANT. (AUT) (°C);
#UMIDADE REL. MAX. NA HORA ANT. (AUT) (%);
#UMIDADE REL. MIN. NA HORA ANT. (AUT) (%);
#UMIDADE RELATIVA DO AR, HORARIA (%);
#VENTO, DIREÇÃO HORARIA (gr) (° (gr));
#VENTO, RAJADA MAXIMA (m/s);
#VENTO, VELOCIDADE HORARIA (m/s);