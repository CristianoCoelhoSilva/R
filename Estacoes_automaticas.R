library(BrazilMet)
library(readr)

estacoes_full_catalog <- read.csv("TEMPERATURA/ESTACOES/CatalogoEstaçõesAutomáticas.csv", sep = ";")

estacoes <- estacoes_full_catalog$CD_ESTACAO

startDate <- "2023-01-01"
endDate <- "2023-12-31"

temperatura_final <- data.frame() 

for (station_code in estacoes) { 
  
  tryCatch({
    temperatura <- download_AWS_INMET_daily(
      stations = c(station_code),
      start_date = startDate,
      end_date = endDate
    )
    
    if (!is.null(temperatura) && nrow(temperatura) > 0) {
      
      temperatura <- temperatura[c(1, 2, 3, 4, 6, 7)]
      
      if (ncol(temperatura_final) == 0) {
        temperatura_final <- temperatura
      } else {
        if (all(names(temperatura) %in% names(temperatura_final)) &&
            all(names(temperatura_final) %in% names(temperatura))) {
          temperatura_final <- rbind(temperatura_final, temperatura)
        } else {
          message(paste("Column names mismatch for station:", station_code, ". Skipping rbind."))
        }
      }
    } else {
      message(paste("No data downloaded or empty data for station:", station_code))
    }
  }, error = function(e) {
    message(paste("Error downloading data for station:", station_code, "-", e$message))
  })
}

