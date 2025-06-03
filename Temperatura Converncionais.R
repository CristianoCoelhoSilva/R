library(readr)
library(dplyr)

diretorio_origem <- paste0('TEMPERATURA/TEMPERATURA/CONVENCIONAIS/2016')
arquivos_para_mover <- list.files(diretorio_origem, pattern = "\\.csv$", full.names = TRUE)

dados_final <- data.frame(
  arquivo = as.character(),
  Data = as.POSIXct(character()),
  temperatura_maxima = numeric(),
  temperatura_minima = numeric(),
  stringsAsFactors = FALSE
)

for (caminho_arquivo in arquivos_para_mover) {
  cat("\nProcessando arquivo:", caminho_arquivo, "\n") # Mensagem mais clara de início
  
  dados <- tryCatch({
    dados_lidos <- read.delim(caminho_arquivo, skip = 10, sep = ";", header = TRUE, dec = ",", fileEncoding = "latin1")
    
    dados_lidos$TEMPERATURA.MEDIA.COMPENSADA..DIARIA.Â.C. <- NULL
    
    # Verificar se dados_lidos não está vazio imediatamente após a leitura
    if (nrow(dados_lidos) == 0) {
      stop("Arquivo lido está vazio após skip ou não contém dados válidos.")
    }
    
    dados_lidos <- dados_lidos[, c(1, 2, 3)]
    colnames(dados_lidos) <- c('Data', 'temperatura_maxima', 'temperatura_minima')
    
    # --- FIX IS HERE ---
    # Assign only the basename of the CURRENT file being processed
    dados_lidos$arquivo <- basename(caminho_arquivo)
    # --- END FIX ---
    
    # Correção de um possível typo: dados_lidos$temperura_minima para dados_lidos$temperatura_minima
    dados_lidos$temperatura_maxima <- as.numeric(gsub(",", ".", dados_lidos$temperatura_maxima))
    dados_lidos$temperatura_minima <- as.numeric(gsub(",", ".", dados_lidos$temperatura_minima))
    
    dados_lidos$Data <- as.POSIXct(dados_lidos$Data, format = "%Y-%m-%d", tz = "UTC")
    
    cat("  Estrutura de dados_lidos do arquivo ATUAL APÓS conversão:\n")
    print(str(dados_lidos))
    cat("  Dimensões de dados_lidos do arquivo ATUAL APÓS conversão (linhas, colunas):", dim(dados_lidos), "\n")
    
    
    dados_lidos # Retorna o data frame processado
  }, error = function(e) {
    cat("  ERRO ao processar o arquivo:", caminho_arquivo, "\n", "  Mensagem de Erro:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(dados)) {
    cat("  Pulando arquivo devido a erro ou dados nulos.\n")
    next
  }
  
  if (nrow(dados) == 0) {
    cat("  Atenção: O arquivo", basename(caminho_arquivo), "foi processado mas resultou em 0 linhas de dados. Não será adicionado.\n")
    next
  }
  
  dados_final <- bind_rows(dados_final, dados)
}
write_csv(dados_final, file = "TEMPERATURA/TEMPERATURA/CONVENCIONAIS/2016.csv")
