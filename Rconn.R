library(shiny)
library(RMySQL)

con <- dbConnect(MySQL(), user = "root", password = "1234", dbname = "world", host = "localhost")

ui <- fluidPage(
  textAreaInput("query", "Inserisci la tua query SQL:"),
  actionButton("execute", "Esegui Query"),
  tableOutput("results")
)

server <- function(input, output) {
  observeEvent(input$execute, {
    query <- input$query
    results <- dbGetQuery(con, query)
    output$results <- renderTable(results)
  })
}

shinyApp(ui, server)
