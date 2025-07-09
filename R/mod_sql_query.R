# ===================================================================
# === R/mod_sql_query.R ===
# ===================================================================

#' SQL Query Module UI
#'
#' Creates the UI for SQL query input and results
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
#' @import shiny
#' @import DT
#' @export
mod_sql_query_ui <- function(id) {
  ns <- NS(id)

  div(
    style = "padding: 2rem;",
    textAreaInput(ns("sql_input"),
                  "SQL-Abfrage (automatisch generiert oder manuell):",
                  value = "",
                  rows = 5,
                  width = "100%"),
    actionButton(ns("run_sql"), "Ausführen", class = "btn-tg"),
    br(), br(),
    DTOutput(ns("custom_query_table"))
  )
}

#' SQL Query Module Server
#'
#' Server logic for SQL query execution
#'
#' @param id Module namespace ID
#' @param chat_module Chat module object
#'
#' @import shiny
#' @import DT
#' @import DBI
#' @export
mod_sql_query_server <- function(id, chat_module) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update SQL input when chat_module$sql() changes
    observe({
      req(chat_module$sql())
      updateTextAreaInput(session, "sql_input", value = chat_module$sql())
    })

    # Execute SQL query
    observeEvent(input$run_sql, {
      req(input$sql_input, chat_module$conn())

      result <- execute_sql_query(chat_module$conn(), input$sql_input)

      if (result$success) {
        output$custom_query_table <- renderDT(result$data)
      } else {
        showNotification("Fehler in der SQL-Abfrage", type = "error")
        output$custom_query_table <- renderDT(data.frame(Fehler = "Ungültige SQL-Abfrage"))
      }
    })
  })
}
