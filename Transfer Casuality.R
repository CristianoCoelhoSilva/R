transferEntropy <- function(data) {

other_vars <- tail(names(data))

data_lr <- data

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

# FunC'C#o auxiliar para adicionar resultados
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
  for (v2 in other_vars) {
    if (v != v2) {
      
      series_from <- data_lr[[v]]
      series_to   <- data_lr[[v2]]
      
      # Shannon
      te_shannon <- transfer_entropy(series_from, series_to,
                                     lx = 1, ly = 1,
                                     type = "quantiles",    # Discretization method
                                     quantiles = c(5, 95),
                                     shuffles = 100,
                                     nboot = 300,
                                     burn = 50,
                                     quiet = TRUE,
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
                                   quiet = TRUE,
                                   seed = NULL,
                                   entropy = "renyi")
      
      
      if (!any(results$From == v & results$To == v2 & results$Method == "shannon")) {
        results <- add_results(results, te_shannon$coef, te_shannon$entropy, v, v2)
      }
      
      if (!any(results$From == v & results$To == v2 & results$Method == "renyi")) {
        results <- add_results(results, te_renyi$coef, te_renyi$entropy, v, v2)
      }
    }
  }
}
 return(results) # Adiciona a linha de retorno
}
