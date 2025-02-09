library(linevis)

if (interactive()) {
  library(shiny)
  #----------------------- Most basic example -----------------
  shinyApp(
    ui = fluidPage(linevisOutput("graph2d")),
    server = function(input, output) {
      output$graph2d <- renderLinevis(
        linevis()
      )
    }
  )
  #----------------------- More advanced example -----------------
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

  ui <- fluidPage(
    linevisOutput("appts",width = "100%", height = "auto"),
    div("Visible window:", textOutput("window", inline = TRUE)),
    tableOutput("table")
  )
  server <- function(input, output) {
    output$appts <- renderLinevis(
      linevis(df_data), env = parent.frame(), quoted = FALSE
    )
    output$window <- renderText(
      paste(input$appts_window[1], "to", input$appts_window[2])
    )
    output$table <- renderTable(
      input$appts_data
    )
  }
  shinyApp(ui, server)
}

