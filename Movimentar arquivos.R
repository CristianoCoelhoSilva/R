# Define o diretório onde os arquivos estão localizados
diretorio <- "C:/Users/cristiano.silva/Documents/PROJETO/DADOS"

# Lista todos os arquivos no diretório
arquivos <- list.files(diretorio)

# Define o padrão para extrair o ano do nome do arquivo
padrao <- "ETLSIM\\.DORES_PB_(\\d{4})_t\\.csv"

# Loop através de cada arquivo encontrado
for (arquivo in arquivos) {
  # Verifica se o nome do arquivo corresponde ao padrão
  correspondencia <- grepl(padrao, arquivo)
  
  if (correspondencia) {
    # Extrai o ano usando uma expressão regular
    ano <- gsub(padrao, "\\1", arquivo)
    
    # Cria o nome da pasta com o ano
    pasta_ano <- file.path(diretorio, ano)
    
    # Verifica se a pasta do ano já existe e a cria se não existir
    if (!dir.exists(pasta_ano)) {
      dir.create(pasta_ano)
      cat(paste("Pasta '", ano, "' criada.\n", sep = ""))
    }
    
    # Constrói os caminhos completos para o arquivo antigo e novo
    caminho_antigo <- file.path(diretorio, arquivo)
    caminho_novo <- file.path(pasta_ano, arquivo)
    
    # Move o arquivo para a pasta do ano
    file.rename(caminho_antigo, caminho_novo)
    cat(paste("Arquivo '", arquivo, "' movido para '", ano, "'.\n", sep = ""))
  }
}
