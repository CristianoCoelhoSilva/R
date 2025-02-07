linevis()
#----------------------- Minimal data -----------------
df_data = data.frame(x = c('2014-06-11',
                           '2014-06-12',
                           '2014-06-13',
                           '2014-06-14',
                           '2014-06-15',
                           '2014-06-16'),
                     y = c(0,
                           1,
                           30000,
                           10,
                           150,
                           30000))
linevis(df_data)

df_data = rbind(df_data, data.frame(x = c('2014-06-09', '2014-06-18'),
                                    y = c(20, 1000)))
df_data$group = c(rep(0, 6), 1, 1)
df_grp = data.frame(id = 0:1, content = c('ESR', 'threshold'),
                    className = c('grp1', 'grp2'))
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
