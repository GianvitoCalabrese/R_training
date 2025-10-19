# select_columns_module.R

# Modulo per la selezione delle colonne
select_columns_ui <- function(id) {
  ns <- NS(id)  # Namespace per il modulo
  tagList(
    selectInput(ns("col1"), "Seleziona la prima colonna (Fattore):", choices = NULL),
    selectInput(ns("col2"), "Seleziona la seconda colonna (Risposta):", choices = NULL)
  )
}

select_columns_server <- function(id, table_data) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(table_data())
      updateSelectInput(session, "col1", choices = colnames(table_data()))
      updateSelectInput(session, "col2", choices = colnames(table_data()))
    })
    return(list(col1 = reactive(input$col1), col2 = reactive(input$col2)))
  })
}
