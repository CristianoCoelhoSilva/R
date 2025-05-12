library(ggplot2)
library(grid)
options(scipen = 999)

lista_nomes <- list(
  "Tatiane Guimarães dos Santos",
  "CHRISTIAN AUGUSTO SILVA",
  "ÍTALO RODRIGO SOARES AGUIAR REIS",
  "DANIEL DE CASTRO BORGES",
  "CRISTIANE MEIRELES ORTIZ",
  "LUCIANO SILVESTRE DA SILVA",
  "LEA FARIAS VEICER LIMA",
  "ALINE VERAS DE ARAUJO",
  "RENILA LACERDA BRAGAGNOLI",
  "ANA CRISTINA DE VASCONCELOS AZEVEDO",
  "ISAÍAS DINIZ NUNES",
  "TASSIO VINÍCIO GUARNIERI FIGNER DE LUNA",
  "HUGO FELIPE DA SILVA LIMA",
  "ANA PAULA MACIEL",
  "BÁRBARA PAIXÃO DE ALENCAR",
  "CARLOS EDUARDO PINTO ROSA CAMBOIM",
  "GUILHERME BARBOSA RODRIGUES FONSECA NAVES",
  "JOSÉ MEDEIROS BARROS NETO",
  "MAIRÃ SOARES SALES",
  "ROSEANE MARIA DE HOLLANDA CAVALCANTI"
)


for (v in lista_nomes) {

X1 <- 2 + 3 * runif(25)
X2 <- 4 + 2 * runif(25)
X3 <- 1 + 1.5 * runif(25)

erro <- rnorm(25, mean = 0, sd = 1) 
Y <- 2 + X1 + X2 + X3 + erro

dados <- data.frame(X1 = X1, X2 = X2, X3 = X3, Y = Y)

modelo <- lm(Y ~ X1 + X2 + X3, data = dados)

arquivo = v

png(arquivo || .pnj, width = 900, height = 700)

summary(modelo)

grid.text(paste(saida_texto, collapse = "\n"),
          x = 0.05, y = 0.95, just = c("left", "top"),
          gp = gpar(fontfamily = "mono", fontsize = 20))  # Fonte maior aqui

dev.off()

}


