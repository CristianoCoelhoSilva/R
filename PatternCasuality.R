install.packages("foreach")
install.packages("doParallel")
library(foreach)
library(doParallel)

PatternCasuality <- function(data) {
  
  # Caso o pacote de erro, executar as funC'C5es diretamente
  # source(here("CASUALIDADE", "CarregarFunctionsCasuality.r"))
  # carregar_scripts_pattern_causality()
  
  other_vars <- tail(names(data), n = 10)  # Specify the number of columns to use, or adjust as needed
  data_lr <- data
  
  # Pre-allocate results data frame
  results_casuality <- data.frame(
    From = character(0),
    To = character(0),
    Method = character(0),
    E_value = numeric(0),
    Total = numeric(0),
    Positive = numeric(0),
    Negative = numeric(0),
    Dark = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Function to add results directly (without rbind)
  add_results <- function(results, metodo, e_value, result, from, to, idx) {
    results[idx, ] <- c(from, to, metodo, e_value, result$total, result$positive, result$negative, result$dark)
    return(results)
  }
  
  # Function to process pairwise combinations
  process_pc <- function(X, Y, metodo, E_values, from, to, idx_start) {
    idx <- idx_start
    for (E in E_values) {
      print(paste("Running pcLightweight for E =", E, "from", from, "to", to))
      tryCatch({
        pc_result <- pcLightweight(X, Y, E = E, tau = 1, metric = metodo, h = 1, weighted = TRUE)
        results_casuality <<- add_results(results_casuality, metodo, E, pc_result, from, to, idx)
        idx <- idx + 1
      }, error = function(e) {
        message("Error in pcLightweight for E = ", E, ": ", e$message)
      })
    }
  }
  
  # Generate pairwise combinations of variables
  combinations <- expand.grid(v = other_vars, v2 = other_vars)
  combinations <- combinations[combinations$v != combinations$v2, ]
  
  # Define the metrics and E_values
  metrics <- c("euclidean")
  E_values <- c(3, 4)
  
  # Pre-calculate the number of results we need to store
  num_results <- nrow(combinations) * length(metrics) * length(E_values)
  idx <- 1  # Index to track rows in the results data frame
  
  # Set up parallel processing using foreach and doParallel
  # Detect number of available cores
  num_cores <- detectCores() - 1  # Leave one core for the system
  
  # Register parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Parallel processing of combinations
  foreach(i = 1:nrow(combinations), .packages = "patterncausality") %dopar% {
    row <- combinations[i, ]
    X1 <- data_lr[[row["v"]]]
    Y1 <- data_lr[[row["v2"]]]
    
    # Apply 'process_pc' for each metric
    for (metodo in metrics) {
      process_pc(X1, Y1, metodo, E_values, row["v2"], row["v"], idx)
    }
  }
  
  # Stop the parallel cluster after finishing the tasks
  stopCluster(cl)
  
  return(results_casuality)
}
