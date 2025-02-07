# Carregar os pacotes necessários
library(forecast)
library(readxl) #Para ler arquivos Excel.
library(ggrepel) #Usado para adicionar rótulos aos gráficos de maneira que eles não se sobreponham.
library(ggthemes) #Pacote com temas adicionais para gráficos ggplot2.
library(ggplot2) #Usado para criar os gráficos.
library(readxl) #Para ler arquivos Excel.
library(tidyr) #Para transformar e reorganizar dados.
library(dplyr) #Para manipulação de dados

# Definir o caminho para o arquivo Excel
arquivo_excel <- "DADOS/setores_brasil.xlsx"

# Ler a planilha
dados <- read_excel(arquivo_excel)

dados <- dados %>%
  pivot_longer(cols = starts_with("MXBR"),  # Seleciona as colunas que começam com "MXBR"
               names_to = "Indice",         # Cria a coluna "Indice" com o nome das colunas antigas
               values_to = "Valor")        # Cria a coluna "Valor" com os valores



#Convertendo as variáveis
dados$Data <- as.Date(dados$dates, format = "%Y-%m-%d")
dados$Valor <- as.numeric(gsub(",", ".", dados$Valor))


# Filtrando os dados onde o 'Indice' é igual a 1
dados <- subset(dados, Indice == 'MXBR Index')
#dados <- dados %>% filter(Data >= "2024-12-01" & Data <= "2025-01-31")

y10 = dados$Valor

plot.ts(y10)

library(urca)

?ur.df

adf_10 <- ur.df(y = y10, type = 'trend', lags = 10, selectlags = 'BIC')

summary(adf_10)
