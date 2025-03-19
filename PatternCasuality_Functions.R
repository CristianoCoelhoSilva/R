set.seed(123)
# Load required libraries
library(readxl)         # For reading Excel files
library(dplyr)          # For data manipulation
library(readxl)
library(patterncausality)

data <- read_excel("C:/modeloives/commodity_2024_final.xlsx")

data_lr <- data %>%
  arrange(Date) %>%
  mutate(across(-1, ~ log(.x) - lag(log(.x)))) %>%
  slice(-1)  # Remove the first row with NA log returns

other_vars <- c("CNETS", "Brent", "Gas", "Corn", "Soybean", "Gold",
                "Iron Ore", "Coal", "Wheat", "Copper")

data_lr <- data_lr %>% select(all_of(other_vars))

X <- data_lr$Gas
Y <- data_lr$Corn

#Implements a computationally efficient version of the Pattern Causality Model Mk. II for analyzing
#causal interactions between two time series. This function uses pattern and signature spaces to
#assess causality through reconstructed state spaces and hashed pattern analysis.
pc_result <- pcLightweight(X, Y
                         , E = 3
                         , tau = 1
                         , metric = "euclidean"
                         , h = 1, weighted = TRUE)

print(pc_result)

#total: Total causality strength
#positive: Proportion of positive causality
#negative: Proportion of negative causality
#dark: Proportion of dark causality

plot_total(pc_result)
plot_components(pc_result)

#Implements an advanced pattern causality algorithm to explore the causal relationships between two
#time series datasets. This function provides comprehensive analysis of causality patterns, including
#state space reconstruction, pattern identification, and causality strength evaluation.
pc_full_details <- pcFullDetails(X, Y
                              , E = 3
                              , tau = 1
                              , metric = "euclidean"
                              , h = 1
                              , weighted = TRUE
                              , distance_fn = NULL
                              , state_space_fn = NULL
                              , verbose = FALSE)

print(pc_full_details)
plot_causality(pc_full_details, 'total')
plot_causality(pc_full_details, 'dark')
plot_causality(pc_full_details, 'positive')
plot_causality(pc_full_details, 'negative')

#Evaluates the robustness of pattern causality measures through repeated sampling analysis. This
#function performs cross-validation by analyzing multiple subsets of the data to assess the stability
#of causality relationships.
cv_validation <- pcCrossValidation(X, Y,
                                  , E = 3 
                                  , tau = 1
                                  , metric = "euclidean"
                                  , h = 1
                                  , weighted = FALSE,
                                  , numberset = c(100, 200, 300, by = 100))

print(cv_validation$results)
plot(cv_validation, fr = FALSE)
plot(cv_validation, separate = TRUE)

cv_validation_bootstrap <- pcCrossValidation(X = X,  Y = Y,
                                             numberset = seq(100, 300, by = 100),
                                             E = 3,
                                             tau = 2,
                                             metric = "euclidean",
                                             h = 1,
                                             weighted = FALSE,
                                             random = TRUE,
                                             bootstrap = 10)

print(cv_validation_bootstrap$results)
plot(cv_validation_bootstrap)
plot(cv_validation_bootstrap, separate = TRUE)


#Analyzes pattern causality relationships between multiple time series by computing pairwise causality measures and organizing them into matrices.
pcmatrix <- pcMatrix(data_lr
                   , E = 3,
                   , tau = 1
                   , metric = "euclidean" 
                   , h = 1
                   , weighted = TRUE)

print(pcmatrix)
plot(pcmatrix, status = "positive", width = 0.85,   height = 0.75, radius = grid::unit(3, "pt"), alpha = 0.53, show_text = TRUE, show_legend_title = FALSE)
plot(pcmatrix, status = "negative", width = 0.85,   height = 0.75, radius = grid::unit(3, "pt"), alpha = 0.53, show_text = TRUE, show_legend_title = FALSE)
plot(pcmatrix, status = "dark", width = 0.85,   height = 0.75, radius = grid::unit(3, "pt"), alpha = 0.53, show_text = TRUE, show_legend_title = FALSE)

#Analyzes pattern causality matrices to compute and summarize the directional effects of different
#causality types (positive, negative, dark) between system components.
effects <- pcEffect(pcmatrix)
print(effects)
plot(effects, status = "negative")
plot(effects, status = "positive")
plot(effects, status = "dark")

#Avalia a precisão da previsão de causalidade em múltiplas séries temporais dentro de um conjunto de dados usando o método PC Mk. II Light.
accuracy <- pcAccuracy(data = data_lr
                     , E = 3
                     , tau = 1
                     , metric = "euclidean" 
                     , h = 1
                     , weighted = TRUE 
                     , verbose = TRUE)
print(accuracy)
summary(accuracy)
