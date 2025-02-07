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
  df_data = data.frame(x = c('2014-06-11',
                             '2014-06-12',
                             '2014-06-13',
                             '2014-06-14',
                             '2014-06-15',
                             '2014-06-16'),
                       y = c(10,
                             1,
                             30000,
                             10,
                             150,
                             30000))
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
