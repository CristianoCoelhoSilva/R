library(linevis)
library(ggrepel)
library(ggthemes)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)

# Definir o caminho para o arquivo Excel
arquivo_excel <- "./DADOS/setores_brasil.xlsx"

# Ler a planilha
dados <- read_excel(arquivo_excel)

dados <- dados %>%
  pivot_longer(cols = starts_with("MXBR"),
               names_to = "Indice",
               values_to = "Valor")

#Convertendo as variáveis
dados$Data <- as.Date(dados$dates, format = "%Y-%m-%d")
dados$Valor <- as.numeric(gsub(",", ".", dados$Valor))

dados1 <- dados %>% filter(Indice == "MXBR Index")

x <- dados1$dates
y <- dados1$Valor

df_data <- data.frame(x = c(x), y = c(y))

## Not run:
linevis() %>%
  zoomIn()
linevis() %>%
  zoomOut(0.3)

## End(Not run)
if (interactive()) {
  library(shiny)
  shinyApp(
    ui = fluidPage(
      linevisOutput("graph2d"),
      sliderInput("zoom", "Zoom by", min = 0, max = 1, value = 0.5, step = 0.1),
      checkboxInput("animate", "Animate?", TRUE),
      actionButton("zoomIn", "Zoom IN"),
      actionButton("zoomOut", "Zoom OUT")
    ),
    server = function(input, output) {
      output$graph2d <- renderLinevis(
        linevis(df_data)
      )
      observeEvent(input$zoomIn, {
        zoomIn("graph2d", percent = input$zoom, animation = input$animate)
      })
      observeEvent(input$zoomOut, {
        zoomOut("graph2d", percent = input$zoom, animation = input$animate)
      })
    }
  )
}
