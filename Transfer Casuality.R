# Load required libraries
library(readxl)         # For reading Excel files
library(dplyr)          # For data manipulation
library(RTransferEntropy)  # For transfer entropy analysis
library(data.table)
library(bit64)
library(tidyverse)
library(stargazer)
library(lfe)
library(lubridate)
library(gridExtra)
library(grid)
library(fixest)
library(openxlsx)
set.seed(1234567890)
options(scipen = 999)
library(writexl)
library(vars)




data <- read_excel("PROJETOS/ANA/Base Final.xlsx")

#Remove linhas com NAs
linhas_completas <- complete.cases(data)
data <- data[linhas_completas, ]

carbono <- c("Preço_Credito_Carbono_Futuros_euro")

indices  <- c("S&P GLOBAL CLEAN ENERGY E - PRICE INDEX",
             "Bloomberg Nat. Gas Idx Eur Hg Dl ER - EXCESS RETURN",
             "CLEAN ENERGY FUELS (FRA)",
             "S&P GSCI Industrial Metals Euro ER - EXCESS RETURN")

data_lr <- data %>%
  arrange(Data) %>%
  mutate(across(-1, ~ log(.x) - lag(log(.x)))) %>%
  slice(-1)  # Remove the first row with NA log returns

data_lr <- data_lr[,-1]

results <- data.frame(
  From = character(),
  To = character(),
  Method = character(),
  Lag = numeric(),
  quantile = character(),
  Q = character(),
  TransferEntropy = numeric(),
  EffTransferEntropy = numeric(),
  StErr = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Função auxiliar para adicionar resultados
add_results <- function(results, coef, method, from, to, nLags, quatiles, q) {
  data <- data.frame(
    From = c(from, to),
    To = c(to, from),
    Method = method,
    Lag = nLags,
    quantile = quatiles,
    Q = q,
    TransferEntropy = coef[,1],
    EffTransferEntropy = coef[,2],
    StErr = coef[,3],
    p_value = coef[,4],
    stringsAsFactors = FALSE
  )
  rbind(results, data)
}

quantiles <- data.frame(q1 = 5:5, q2 = 95:95)
quantiles$quantiles <- factor(sprintf("(%02.f, %02.f)", quantiles$q1, quantiles$q2))

q_reny <- data.frame(
  q = c(0.1, 0.3, 0.5, 0.8)
)

max_lags <- 10

for (v in carbono) {
  for (v2 in indices) {
    
    #selecionar o melhor AIC or BIC
    for (v in carbono) {
      for (v2 in indices) {
        
        dados_xy <- data_lr[, c(v, v2)]
        
        selecao_lags <- VARselect(dados_xy, lag.max = max_lags, type = "const")

        ordem_aic <- selecao_lags$selection["AIC(n)"]
        ordem_bic <- selecao_lags$selection["SC(n)"] 
      }
    }
 
      series_from <- data_lr[[v]]
      series_to   <- data_lr[[v2]]

        for (i in 1:nrow(quantiles)) {
          
          te_shannon <- transfer_entropy(series_from, 
                                         series_to,
                                         lx = ordem_aic, 
                                         ly = ordem_aic,
                                         type = "quantiles",
                                         quantiles = c(quantiles$q1[i], quantiles$q2[i]),
                                         shuffles = 100,
                                         nboot = 300,
                                         burn = 50,
                                         quiet = FALSE,
                                         seed = '123',
                                         entropy = "shannon")
          
        results <- add_results(results, te_shannon$coef, te_shannon$entropy, v, v2, ordem_aic, quantiles$quantiles[i], 'NULL')
          
        for (q in 1:nrow(q_reny))  {
        te_renyi <- transfer_entropy(series_from,
                                         series_to,
                                         lx = ordem_aic,
                                         ly = ordem_aic,
                                         type = "quantiles",
                                         quantiles = c(quantiles$q1[i], quantiles$q2[i]),
                                         shuffles = 100,
                                         nboot = 300,
                                         burn = 50,
                                         quiet = FALSE,
                                         seed = 123,
                                         entropy = "Renyi",
                                         q = 0.3)
          
          results <- add_results(results, te_renyi$coef, te_renyi$entropy, v, v2, ordem_aic, quantiles$quantiles[i], q_reny$q)
         }
        }
      }
      write_xlsx(results, path = "~/FINANÇAS/PROJETOS/ANA/RESULTS/TransferEntropy.xlsx")
  }
