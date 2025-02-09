# Definir o caminho para o arquivo Excel
arquivo_excel <- "DADOS/setores_brasil.xlsx"

# Ler a planilha
dados <- read_excel(arquivo_excel)

dados <- dados %>%
  pivot_longer(cols = starts_with("MXBR"),  # Seleciona as colunas que começam com "MXBR"
               names_to = "Indice",         # Cria a coluna "Indice" com o nome das colunas antigas
               values_to = "Valor")        # Cria a coluna "Valor" com os valores

#Convertendo as variáveis
dados$Data <- as.Date(dados$dates, format = "%Y-%m-%d")
dados$Valor <- as.numeric(gsub(",", ".", dados$Valor))

dados1 <- dados %>% filter(Indice == "MXBR Index")

x = dados1$dates
y = dados1$Valor

df_data = data.frame(x = c(x), y = c(y))


if (interactive()) {
  library(shiny)
  shinyApp(
    ui = fluidPage(
    linevisOutput("graph2d"),
    actionButton("btn0", "Add custom time"),
    actionButton("btn", "Remove custom time bar")
  ),
  server = function(input, output) {
    output$graph2d <- renderLinevis(
      linevis(df_data)
    )
    observeEvent(input$btn0, {
      addCustomTime("graph2d", Sys.Date() - 100, "yesterday")
    })
    observeEvent(input$btn, {
      removeCustomTime("graph2d", "yesterday")
    })
  }
  )
}

