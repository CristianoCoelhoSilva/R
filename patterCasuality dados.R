# Carregar os pacotes necessários
rm(list = ls())
options(scipen = 999)
library(readxl) 
library(tidyr)
library(dplyr)
library(urca)
library(readr)
library(data.table)

##Dados Bitcoin
#BTC_BRL <- read_csv("DADOS/BTC_BRL.csv")
#fwrite(BTC_BRL, "./DADOS/BTC_BRL_2015.csv")

#BTC_BRL <-  read_excel('DADOS/BTC_BRL.xlsx')
#BTC_BRL$Data <- as.Date(BTC_BRL$Data, format = "%d.%m.%Y")
#BTC_BRL$Maxima <- as.numeric(gsub(",", ".", BTC_BRL$Maxima))
#BTC_BRL$Maxima <- c(NA, diff(log(BTC_BRL$Maxima)))

##Dados Index
dados <- read_excel('DADOS/dados.xlsx')
dataset <- dados[, c(3, 4, 5, 6, 27)]
dataset <- na.omit(dataset)

dataset$`LEGATRUU Index` <- c(NA, diff(log(dataset$`LEGATRUU Index`)))
dataset$`LBUSTRUU Index` <- c(NA, diff(log(dataset$`LBUSTRUU Index`)))
dataset$`LUACTRUU Index` <- c(NA, diff(log(dataset$`LUACTRUU Index`)))
dataset$`LF98TRUU Index` <- c(NA, diff(log(dataset$`LF98TRUU Index`)))
dataset$ultimo <- c(NA, diff(log(dataset$ultimo)))

X <- dataset$`LEGATRUU Index`
Y <- dataset$ultimo

