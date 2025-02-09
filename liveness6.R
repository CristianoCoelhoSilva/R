library(ggrepel) #Usado para adicionar rótulos aos gráficos de maneira que eles não se sobreponham.
library(ggthemes) #Pacote com temas adicionais para gráficos ggplot2.
library(ggplot2) #Usado para criar os gráficos.
library(readxl) #Para ler arquivos Excel.
library(tidyr) #Para transformar e reorganizar dados.
library(dplyr) #Para manipulação de dados
library(linevis)
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

dados1 <- dados %>% filter(Indice == "MXBR0TC Index")

x = dados1$dates
y = dados1$Valor

df_data = data.frame(x = c(x), y = c(y))

num_linhas <- nrow(df_data)

dados2 <- dados %>% filter(Indice == "MXBR0IT Index")
x = dados2$dates
y = dados2$Valor

df_data = rbind(df_data, data.frame(x = c(x), y = c(y)))


df_data$group = c(rep(0, num_linhas), rep(1, num_linhas))
df_grp = data.frame(id = 0:1, content = c('MXBR0TC Index', 'MXBR0IT Index'),
                    className = c('grp1', 'grp2'))


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
      linevisOutput("appts", width = "100%", height = "auto"),
      linevisOutput("graph2d", width = "100%", height = "auto"),
      sliderInput("zoom", "Zoom by", min = 0, max = 1, value = 0.5, step = 0.1),
      checkboxInput("animate", "Animate?", TRUE),
      actionButton("zoomIn", "Zoom IN"),
      actionButton("zoomOut", "Zoom OUT"),
      actionButton("btn0", "Add custom time"),
      actionButton("btn", "Remove custom time bar"),
      div("Visible window:", textOutput("window", inline = TRUE)),
      tableOutput("table")
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
      observeEvent(input$btn0, {
        addCustomTime("graph2d", Sys.Date() - 100, "yesterday")
      })
      observeEvent(input$btn, {
        removeCustomTime("graph2d", "yesterday")
      })
      output$window <- renderText(
        paste(input$appts_window[1], "to", input$appts_window[2])
      )
      output$table <- renderTable(
        input$appts_data
      )
    }
  )
}

