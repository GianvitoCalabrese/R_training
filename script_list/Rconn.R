library(shiny)
library(RMySQL)

con <- dbConnect(MySQL(), user = "root", password = "fadetoblack", dbname = "world", host = "localhost")

ui <- fluidPage(
  textAreaInput("query", "Inserisci la tua query SQL:"),
  actionButton("execute", "Esegui Query"),
  actionButton("disconnect", "Chiudi Connessione"),
  tableOutput("results")
)

server <- function(input, output, session) {
  observeEvent(input$execute, {
    query <- input$query
    results <- dbGetQuery(con, query)
    output$results <- renderTable(results)
  })
  
  observeEvent(input$disconnect, {
    dbDisconnect(con)
    showNotification("Connessione chiusa", type = "message")
  })
}

shinyApp(ui, server)
