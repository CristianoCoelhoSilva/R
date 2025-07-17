library(readxl)

data <- read_excel("Combatendo as Mudanças Climáticas.xlsx", .name_repair = "minimal")

data[complete.cases(data), ]

nomes <- names(data)
tabela_repeticoes <- as.data.frame(table(nomes))


colunas_repetidas <- subset(tabela_repeticoes, Freq > 1)
colunas_repetidas <- colunas_repetidas[colunas_repetidas$Freq < 10, ]

for(nome_coluna in colunas_repetidas$nomes) {

  indices <- which(names(data) == nome_coluna)
  
  mat <- as.matrix(data[, indices])
  
  primeiro_valido <- apply(mat, 1, function(x) {
    validos <- x[!is.na(x) & x != ""]
    if(length(validos) == 0) return(NA) else return(validos[1])
  })
  
  data <- data[, -indices]
  
  data[[nome_coluna]] <- primeiro_valido
}

primeira_linha <- sapply(data[1, ], as.character)

filtro1 <- !is.na(primeira_linha) & primeira_linha == "Response"
filtro2 <- !is.na(primeira_linha)
filtro3 <- !is.na(primeira_linha) & primeira_linha != "Response"

colunas_unicas <- data[, filtro]
colunas_ns <- data[, filtro2]
colunas_varias <- data[, filtro3]
