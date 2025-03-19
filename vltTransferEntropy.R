rm(list = ls())
# Load required libraries
library(readxl)         # For reading Excel files
library(dplyr)          # For data manipulation
library(VLTimeCausality)  # For transfer entropy analysis
library(readxl)

data <- read_excel("C:/modeloives/commodity_2024_final.xlsx")

data_lr <- data %>%
  arrange(Date) %>%
  mutate(across(-1, ~ log(.x) - lag(log(.x)))) %>%
  slice(-1)  # Remove the first row with NA log returns

# List of "other" variables (excluding CNETS)
other_vars <- c("Brent", "Gas", "Corn", "Soybean", "Gold",
                "Iron Ore", "Coal", "Wheat", "Copper")

# 4. Run VL Transfer Entropy only for pairs:
#    (CNETS -> each other variable) and (each other variable -> CNETS)
# Initialize a data frame to store results
results <- data.frame(
  From = character(),
  To = character(),
  Llag = numeric(),
  Method = character(),
  TransferEntropy = numeric(),
  EffTransferEntropy = numeric(),
  StErr = numeric(),
  p_value = numeric(),
  follVal =  numeric(),
  optCor =  numeric(),
  TEratio =  numeric(),
  stringsAsFactors = FALSE
)

# Função auxiliar para adicionar resultados
add_results <- function(results, vltransferentropy, from, to, lag) {
  data <- data.frame(
    From = c(from, to),
    To = c(to, from),
    Llag = lag,
    TransferEntropy = vltransferentropy$res$coef[,1],
    EffTransferEntropy = vltransferentropy$res$coef[,2],
    StErr = vltransferentropy$res$coef[,3],
    p_value = vltransferentropy$res$coef[,4],
    follVal = vltransferentropy$follOut$follVal,
    optCor = vltransferentropy$follOut$optCor,
    TEratio = vltransferentropy$TEratio,
    stringsAsFactors = FALSE
    #print(out$follOut$ccfout)
  )
  rbind(results, data)
}

for (v in other_vars) {
  # construct two time-series
  set.seed(1234567890)
  
  series_from <- data_lr[["CNETS"]]
  series_to   <- data_lr[[v]]
  
  for (lag in 1:4) {
  
    vltransferentropy <- VLTransferEntropy(X=series_from, Y=series_to, nboot = 300, maxLag=lag, VLflag = TRUE)

    results <- add_results(results, vltransferentropy, "CNETS", v, lag)
  }
}

# 5. Print the final results table
print(results)
