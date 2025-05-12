library(ggplot2)
library(grid)
library(gridExtra) # Para grid.arrange
library(tidyr)
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


if (!dir.exists("RESULTADOS")) {
  dir.create("RESULTADOS")
}

for (v in lista_nomes) {
  
  X1 <- 2 + 3 * runif(30)
  X2 <- 4 + 2 * runif(30)
  X3 <- 1 + 1.5 * runif(30)
  
  erro <- rnorm(30, mean = 0, sd = 1)
  Y <- 2 + X1 + X2 + X3 + erro
  
  dados <- data.frame(X1 = X1, X2 = X2, X3 = X3, Y = Y)
  
  modelo <- lm(Y ~ X1 + X2 + X3, data = dados)
  
  arquivo_nome_pdf <- file.path("RESULTADOS", paste0(gsub(" ", "_", v), "_relatorio_unico.pdf"))
  pdf(arquivo_nome_pdf, width = 8.5, height = 11)
  
  grid.newpage()
  
  pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 1, heights = unit(c(0.4, 0.6), "npc"))))
  
  saida_texto <- capture.output(summary(modelo))
  grid.text(paste("Sumário do Modelo para:", v, "\n", paste(saida_texto, collapse = "\n")),
            x = 0.05, y = 0.98, just = c("left", "top"),
            gp = gpar(fontfamily = "mono", fontsize = 8),
            vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  
  dados_longo <- tidyr::pivot_longer(
    dados,
    cols = starts_with("X"),
    names_to = "Variavel_Independente",
    values_to = "Valor_Independente"
  )
  
  plot_unico <- ggplot(dados_longo, aes(x = Valor_Independente, y = Y)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 0.5) +
    facet_wrap(~ Variavel_Independente, scales = "free_x") +
    labs(
      title = paste("Relação entre Y e Variáveis Independentes para", v),
      x = "Valor da Variável Independente",
      y = "Y"
    ) +
    theme_bw(base_size = 8)
  
  # --- Imprimir o gráfico no viewport inferior ---
  print(plot_unico, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  
  popViewport()
  
  dev.off()
  
}