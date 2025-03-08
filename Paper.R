# Carregar os pacotes necessários
rm(list = ls())
options(scipen = 999)

library(readxl) 
library(tidyr)
library(dplyr)
library(urca)
library(readr)
library(data.table)
library(moments)
library(tseries)
library(readxl)
library(moments)
library(tseries)

path_file = "~/FINANCAS/DADOS/DATA_COMMODITIES.xlsx"

source("~/FINANCAS/Summary Full.R")

calcular_estatisticas_excel(path_file)