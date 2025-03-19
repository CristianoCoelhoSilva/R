rm(list = ls())
# Load required libraries
library(readxl)         # For reading Excel files
library(dplyr)          # For data manipulation
#library(patterncausality)  # Pattern Casuality
library(readxl)
library(here)

#Caso o pacote de erro, executar as funções diretamente
source(here("CASUALIDADE","CarregarFunctionsCasuality.r"))
carregar_scripts_pattern_causality()

data <- read_excel("C:/modeloives/commodity_2024_final.xlsx")

data_lr <- data %>%
  arrange(Date) %>%
  mutate(across(-1, ~ log(.x) - lag(log(.x)))) %>%
  slice(-1)  # Remove the first row with NA log returns

# List of "other" variables (excluding CNETS)
other_vars <- c("Brent", "Gas", "Corn", "Soybean", "Gold",
                "Iron Ore", "Coal", "Wheat", "Copper")

results_casuality <- data.frame(
  From = character(),
  To = character(),
  Method = character(),
  E = numeric(),
  Total = numeric(),
  Positive = numeric(),
  Negative = numeric(),
  Dark = numeric()
)

# Função auxiliar para adicionar resultados
add_results <- function(results, metodo, e_value, result, from, to) {
  data <- data.frame(
    From = from,
    To = to,
    Method = metodo,
    E_value = e_value,
    Total = result$total,
    Positive = result$positive,
    Negative = result$negative,
    Dark = result$dark
  )
  rbind(results_casuality, data)
}

process_pc <- function(X, Y, metodo, E_values, from, to) {
  for (E in E_values) {
    pc_result <- pcLightweight(X, Y, E = E, tau = 1, metric = metodo, h = 1, weighted = TRUE)
    results_casuality <<- add_results(results, metodo, E, pc_result, from, to)
  }
}

for (v in other_vars) {
  set.seed(123)
  X1 <- data_lr[["CNETS"]]
  Y1 <- data_lr[[v]]
  X2 <- data_lr[[v]]
  Y2 <- data_lr[["CNETS"]]

  E_values <- c(3, 4)
  metrics <- c("euclidean","manhattan")
  
  for (metodo in metrics) {
    process_pc(X1, Y1, metodo, E_values, "CNETS", v)
    process_pc(X2, Y2, metodo, E_values, v, "CNETS")
  }
}

print(results)
