# Carregar os pacotes necessários
rm(list = ls())
options(scipen = 999)
library(vrtest)
library(readxl) 
library(tidyr)
library(dplyr)
library(urca)
library(patterncausality)
library(readr)
library(data.table)
library(vrtest)
library(tseries)

log_prices <- read_excel('DADOS/setores_brasil.xlsx')
log_prices <- log(log_prices$`MXBR0FN Index`)  
log_returns <- diff(log_prices)

VR_result <- Lo.Mac(log_returns, c(2, 4, 8, 16))

print(VR_result)
