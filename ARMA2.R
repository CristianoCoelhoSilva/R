library(ggrepel)
library(ggthemes) #Pacote com temas adicionais para gráficos ggplot2.
library(ggplot2) #Usado para criar os gráficos.
library(readxl) #Para ler arquivos Excel.
library(tidyr) #Para transformar e reorganizar dados.
library(dplyr) #Para manipulação de dados

# Definir o caminho para o arquivo Excel
arquivo_excel <- "../DADOS/setores_brasil.xlsx"

# Ler a planilha
dados <- read_excel(arquivo_excel)

dados <- dados %>%
  pivot_longer(cols = starts_with("MXBR"),
               names_to = "Indice",
               values_to = "Valor")

#Convertendo as variáveis
dados$Data <- as.Date(dados$dates, format = "%Y-%m-%d")
dados$Valor <- as.numeric(gsub(",", ".", dados$Valor))

#data_inicial <- as.Date("2022-02-01")
#data_final <- as.Date("2022-02-28")

dados <- dados %>%
  filter(Indice %in% c("MXBR Index"))

library(tseries)


help(arima)

# Ajustar um modelo ARMA para os dados
modelo <- arima(dados$Valor, order = c(1, 0, 1))

# Extrair os coeficientes estimados
C <- modelo$coef["intercept"]
AR_1 <- modelo$coef["ar1"]
MA_1 <- modelo$coef["ma1"]
SIGMASQ <- modelo$sigma2

std_erros <- sqrt(diag(modelo$var.coef))
t_statistic <- C / std_erros
prob <- 2 * (1 - pnorm(abs(t_statistic)))

# Criando um data frame com os valores calculados
resultados <- data.frame(
  Variable = c("C", "AR(1)", "MA(1)", "SIGMASQ"),
  Coefficient = c(C, AR_1, MA_1, SIGMASQ),
  Std_Error = c(std_erros["intercept"], std_erros["ar1"], std_erros["ma1"], NA),
  t_Statistic = c(t_statistic["intercept"], t_statistic["ar1"], t_statistic["ma1"], NA),
  Prob = c(prob["intercept"], prob["ar1"], prob["ma1"], NA)
)

# Exibindo os valores calculados
print(resultados)
