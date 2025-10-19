library(shiny)
library(DT)
library(ggplot2)
library(RMySQL)

# Carica il modulo per la selezione delle colonne
source("select_columns_module.R")

# Connessione al database MySQL
#con <- dbConnect(MySQL(), user = "root", password = "fadetoblack", dbname = "world", host = "localhost")

# Modulo per gestire l'importazione dei dati CSV e query SQL
import_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Carica un file CSV"),
    fileInput(ns("file"), "Scegli il file CSV",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput(ns("header"), "Intestazioni", TRUE),
    radioButtons(ns("sep"), "Separatore",
                 choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                 selected = ","),
    radioButtons(ns("quote"), "Virgolette",
                 choices = c(None = "", "Doppie virgolette" = '"', "Virgolette singole" = "'"),
                 selected = '"'),
    actionButton(ns("saveTable"), "Salva Tabella CSV"),
    hr(),
    h4("Oppure esegui una query SQL"),
    textAreaInput(ns("query"), "Inserisci la tua query SQL:"),
    actionButton(ns("execute"), "Esegui Query"),
    tableOutput(ns("results"))
  )
}

import_data_server <- function(id, tableList, session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Funzionalità per caricare e salvare il file CSV
    observeEvent(input$saveTable, {
      req(input$file)
      newTable <- read.csv(input$file$datapath, 
                           header = input$header, 
                           sep = input$sep, 
                           quote = input$quote)
      
      tableName <- paste("Table", length(tableList$tables) + 1)
      tableList$tables[[tableName]] <- newTable
      
      # Debug: verifica la tabella salvata
      print(paste("Tabella salvata:", tableName))
      print(head(newTable))
      
      updateSelectInput(session, "selectedTable", choices = names(tableList$tables))
    })
    
    # Funzionalità per eseguire la query SQL
    observeEvent(input$execute, {
      query <- input$query
      results <- dbGetQuery(con, query)
      output$results <- renderTable(results)
    })
  })
}

ui <- fluidPage(
  titlePanel("Gestione di Più Tabelle con ANOVA in Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Importa Dati",
                 import_data_ui("import_module")  # Modulo per importare file CSV o eseguire query
        ),
        tabPanel("Analisi",
                 selectInput("selectedTable", "Seleziona la tabella", choices = NULL),  # Inizialmente nessuna scelta
                 select_columns_ui("col_selector"),
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
  tableList <- reactiveValues(tables = list())
  
  # Modulo per importare file o eseguire query
  import_data_server("import_module", tableList, session)
  
  # Aggiorna `selectInput` ogni volta che una nuova tabella viene caricata
  observe({
    updateSelectInput(session, "selectedTable", choices = names(tableList$tables))
  })
  
  # Visualizza la tabella selezionata
  output$table <- renderDT({
    req(input$selectedTable)
    
    if (!is.null(tableList$tables[[input$selectedTable]])) {
      print(paste("Visualizzazione della tabella:", input$selectedTable))  # Debug per verificare la tabella selezionata
      datatable(tableList$tables[[input$selectedTable]], options = list(pageLength = 10, autoWidth = TRUE))
    } else {
      print("Tabella selezionata non esistente o NULL")
      return(NULL)
    }
  })
  
  # Seleziona le colonne tramite il modulo
  selected_columns <- select_columns_server("col_selector", reactive({
    req(input$selectedTable)
    tableList$tables[[input$selectedTable]]
  }))
  
  # Esegui ANOVA
  observeEvent(input$runAnova, {
    req(selected_columns$col1(), selected_columns$col2(), input$selectedTable)
    
    data <- tableList$tables[[input$selectedTable]]
    if (!is.null(data)) {
      print("Esecuzione ANOVA su tabella:")
      print(input$selectedTable)
      print(paste("Colonna dipendente:", selected_columns$col2()))
      print(paste("Colonna indipendente:", selected_columns$col1()))
      
      formula <- as.formula(paste(selected_columns$col2(), "~", selected_columns$col1()))
      result <- aov(formula, data = data)
      
      output$anovaResult <- renderPrint({
        summary(result)
      })
      
      # Visualizza il grafico solo se i dati esistono
      output$plot <- renderPlot({
        ggplot(data, aes_string(x = selected_columns$col1(), y = selected_columns$col2())) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = "Boxplot per ANOVA", x = selected_columns$col1(), y = selected_columns$col2())
      })
    }
  })
  
  # Cancellare la tabella selezionata
  observeEvent(input$deleteTable, {
    req(input$selectedTable)
    
    print(paste("Cancellazione della tabella:", input$selectedTable))  # Debug per verificare la cancellazione
    
    tableList$tables[[input$selectedTable]] <- NULL
    updateSelectInput(session, "selectedTable", choices = names(tableList$tables))
    
    # Reset visualizzazione
    output$anovaResult <- renderPrint(NULL)
    output$plot <- renderPlot(NULL)
    output$table <- renderDT(NULL)
  })
}

shinyApp(ui = ui, server = server)
