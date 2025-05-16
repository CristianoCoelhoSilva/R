library(readr)

TEMPERATURA <- read_csv("TEMPERATURA/DADOS/BASE_FINAL_TEMPERATURA.csv")
TEMPERATURA$IBGE = floor(TEMPERATURA$IBGE / 10)

MORTALIDADE <-  read_csv("TEMPERATURA/DADOS/MORTALIDADE/MORTALIDADE_2008_2019.csv")
indices_7_digitos <- nchar(as.character(MORTALIDADE$dtobito)) == 7
MORTALIDADE$dtobito[indices_7_digitos] <- paste0("0", MORTALIDADE$dtobito[indices_7_digitos])
MORTALIDADE$dtobito <- as.Date(as.character(MORTALIDADE$dtobito), format = "%d%m%Y")

basefinal <- inner_join(TEMPERATURA, MORTALIDADE, by = c("IBGE" = "codmunres", "data" = "dtobito"), relationship = "many-to-many")

rm(TEMPERATURA)
rm(MORTALIDADE)


nome_arquivo_saida <- paste0('TEMPERATURA/DADOS/BASE_FINAL.csv')
write.csv(basefinal, file = nome_arquivo_saida, row.names = FALSE, quote = TRUE, na = "")