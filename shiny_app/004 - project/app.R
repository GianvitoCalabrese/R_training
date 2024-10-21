library(shiny)
library(DT)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Gestione di Più Tabelle con ANOVA in Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Importa Dati",
                 fileInput("file", "Scegli il file CSV",
                           accept = c("text/csv", 
                                      "text/comma-separated-values,text/plain", 
                                      ".csv")),
                 checkboxInput("header", "Intestazioni", TRUE),
                 radioButtons("sep", "Separatore",
                              choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                              selected = ","),
                 radioButtons("quote", "Virgolette",
                              choices = c(None = "", 
                                          "Doppie virgolette" = '"', 
                                          "Virgolette singole" = "'"),
                              selected = '"'),
                 actionButton("saveTable", "Salva Tabella")
        ),
        tabPanel("Analisi",
                 selectInput("selectedTable", "Seleziona la tabella", choices = NULL),
                 uiOutput("colSelect1"),
                 uiOutput("colSelect2"),
                 actionButton("runAnova", "Esegui ANOVA"),
                 verbatimTextOutput("anovaResult"),
                 actionButton("deleteTable", "Cancella Tabella Selezionata")
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dati Importati", DTOutput("table")),
        tabPanel("Grafico", plotOutput("plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Lista per salvare più dataset
  tableList <- reactiveValues(tables = list())
  
  # Carica il file e lo aggiungi alla lista di tabelle
  observeEvent(input$saveTable, {
    req(input$file)
    newTable <- read.csv(input$file$datapath, 
                         header = input$header, 
                         sep = input$sep, 
                         quote = input$quote)
    
    tableName <- paste("Table", length(tableList$tables) + 1)
    tableList$tables[[tableName]] <- newTable
    
    # Aggiorna la lista di tabelle disponibili per l'analisi
    updateSelectInput(session, "selectedTable", choices = names(tableList$tables))
  })
  
  # Visualizza la tabella selezionata
  output$table <- renderDT({
    req(input$selectedTable)
    datatable(tableList$tables[[input$selectedTable]], options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Output per le selezioni delle colonne
  output$colSelect1 <- renderUI({
    req(input$selectedTable)
    selectInput("col1", "Seleziona la prima colonna (Fattore):", choices = colnames(tableList$tables[[input$selectedTable]]))
  })
  
  output$colSelect2 <- renderUI({
    req(input$selectedTable)
    selectInput("col2", "Seleziona la seconda colonna (Risposta):", choices = colnames(tableList$tables[[input$selectedTable]]))
  })
  
  # Esegui l'ANOVA
  observeEvent(input$runAnova, {
    req(input$col1, input$col2, input$selectedTable)
    data <- tableList$tables[[input$selectedTable]]
    
    # Formula per ANOVA
    formula <- as.formula(paste(input$col2, "~", input$col1))
    
    # Esegui ANOVA
    result <- aov(formula, data = data)
    output$anovaResult <- renderPrint({
      summary(result)
    })
    
    # Visualizzazione del grafico (boxplot)
    output$plot <- renderPlot({
      ggplot(data, aes_string(x = input$col1, y = input$col2)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Boxplot per ANOVA", x = input$col1, y = input$col2)
    })
  })
  
  # Cancellare la tabella selezionata
  observeEvent(input$deleteTable, {
    req(input$selectedTable)
    tableList$tables[[input$selectedTable]] <- NULL
    
    # Aggiorna la lista delle tabelle
    updateSelectInput(session, "selectedTable", choices = names(tableList$tables))
    
    # Reset visualizzazione
    output$anovaResult <- renderPrint(NULL)
    output$plot <- renderPlot(NULL)
    output$table <- renderDT(NULL)
  })
}

# Esegui l'app
shinyApp(ui = ui, server = server)
