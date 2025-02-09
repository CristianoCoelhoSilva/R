library(ggrepel) #Usado para adicionar rótulos aos gráficos de maneira que eles não se sobreponham.
library(ggthemes) #Pacote com temas adicionais para gráficos ggplot2.
library(ggplot2) #Usado para criar os gráficos.
library(readxl) #Para ler arquivos Excel.
library(tidyr) #Para transformar e reorganizar dados.
library(dplyr) #Para manipulação de dados

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

num_linhas <- nrow(df_data)

dados2 <- dados %>% filter(Indice == "MXBR0IT Index")
x = dados2$dates
y = dados2$Valor

df_data = rbind(df_data, data.frame(x = c(x), y = c(y)))


df_data$group = c(rep(0, 3697), rep(1, 3697))
df_grp = data.frame(id = 0:1, content = c('ESR', 'threshold'),
                    className = c('grp1', 'grp2'))

linevis()
linevis(df_data, df_grp)


if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    linevisOutput("appts"),
    div("Visible window:", textOutput("window", inline = TRUE)),
    tableOutput("table")
  )
  server <- function(input, output) {
    output$appts <- renderLinevis(
      linevis(df_data)
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

