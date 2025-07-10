#' Search Module UI
#'
#' Creates the UI for the dataset search functionality
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
#' @import shiny
#' @import shinyjs
#' @import waiter
#' @export
mod_search_ui <- function(id) {
  ns <- NS(id)

  div(id = "search_ui",
      div(class = "centered-box",
          style = "
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          text-align: center;
          padding: 2rem;
          ",
          h1("OGD-Chatbot Thurgau", style = "margin-bottom: 0.5rem;"),
          p("Thurgauer Open Government Data (OGD) direkt befragen – OGD Katalog durchsuchen, Daten auswählen und direkt über den KI-Chatbot Fragen dazu stellen.",
            style = "margin-bottom: 1.5rem; max-width: 600px;"
          ),
          create_disclaimer_text(),
          textInput(ns("search_term"), label = NULL, placeholder = "Datensatz suchen...", width = "400px"),
          actionButton(ns("search"), "Suchen", class = "btn-tg"),

          # Container for results with spinner
          div(id = ns("results_container"),
              uiOutput(ns("results_ui"))
          )
      )
  )
}



#' Search Module Server
#'
#' Server logic for the dataset search functionality
#'
#' @param id Module namespace ID
#'
#' @return List with selected_dataset reactive
#' @import shiny
#' @import waiter
#' @import jsonlite
#' @export
mod_search_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    catalog <- get_non_geodata()
    # Reactive values
    selected_dataset <- reactiveVal(NULL)

    # Initialize waiter
    w <- Waiter$new(
      id = ns("results_container"),
      html = tagList(
        bs5_spinner(),
        h3("Datensätze werden gesucht...", style = "color: #327A1E; margin-top: 20px;")
      ),
      color = "transparent"
    )

    # Search results
    search_results <- eventReactive(input$search, {
      req(nchar(input$search_term) > 1)

      w$show()

      tryCatch({
        results <- search_datasets(input$search_term)
        return(results)
      }, error = function(e) {
        showNotification("Fehler bei der Suche", type = "error")
        return(NULL)
      })
    })

    # Render search results
    output$results_ui <- renderUI({
      req(search_results())

      search_results_df <- process_search_results(search_results(),catalog)

      w$hide()

      if (nrow(search_results_df) == 0) {
        return(create_no_results_ui())
      }

      create_results_cards(search_results_df, ns)
    })

    # Handle card clicks
    observeEvent(input$card_clicked, {
      selected_dataset(input$card_clicked)
    })

    # Return values
    list(
      selected_dataset = selected_dataset
    )
  })
}
