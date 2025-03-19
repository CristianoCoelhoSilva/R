# Load required libraries
library(readxl)         # For reading Excel files
library(dplyr)          # For data manipulation
library(RTransferEntropy)  # For transfer entropy analysis
library(readxl)

data <- read_excel("C:/modeloives/commodity_2024_final.xlsx")

data_lr <- data %>%
  arrange(Date) %>%
  mutate(across(-1, ~ log(.x) - lag(log(.x)))) %>%
  slice(-1)  # Remove the first row with NA log returns

# List of "other" variables (excluding CNETS)
other_vars <- c("Brent", "Gas", "Corn", "Soybean", "Gold",
                "Iron Ore", "Coal", "Wheat", "Copper")

# 4. Run Transfer Entropy only for pairs:
#    (CNETS -> each other variable) and (each other variable -> CNETS)
# Initialize a data frame to store results
results <- data.frame(
  From = character(),
  To = character(),
  Method = character(),
  TransferEntropy = numeric(),
  EffTransferEntropy = numeric(),
  StErr = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Função auxiliar para adicionar resultados
add_results <- function(results, coef, method, from, to) {
  data <- data.frame(
    From = c(from, to),
    To = c(to, from),
    Method = method,
    TransferEntropy = coef[,1],
    EffTransferEntropy = coef[,2],
    StErr = coef[,3],
    p_value = coef[,4],
    stringsAsFactors = FALSE
  )
  rbind(results, data)
}

for (v in other_vars) {
  # construct two time-series
  set.seed(1234567890)
  
  series_from <- data_lr[["CNETS"]]
  series_to   <- data_lr[[v]]
  
  # Shannon
  te_shannon <- transfer_entropy(series_from, series_to,
                                 lx = 1, ly = 1,
                                 type = "quantiles",    # Discretization method
                                 quantiles = c(5, 95),
                                 shuffles = 100,
                                 nboot = 300,
                                 burn = 50,
                                 quiet = FALSE,
                                 seed = NULL,
                                 entropy = "shannon")
  # Renyi
  te_renyi <- transfer_entropy(series_from, series_to,
                               lx = 1, ly = 1,
                               type = "quantiles",
                               quantiles = c(5, 95),
                               shuffles = 100,
                               nboot = 300,
                               burn = 50,
                               quiet = FALSE,
                               seed = NULL,
                               entropy = "renyi")
  
  
  # Processando te_shannon
  results <- add_results(results, te_shannon$coef, te_shannon$entropy, "CNETS", v)
  
  # Processando te_renyi
  results <- add_results(results, te_renyi$coef, te_renyi$entropy, "CNETS", v)
}

# 5. Print the final results table
print(results)

packageVersion("RTransferEntropy")