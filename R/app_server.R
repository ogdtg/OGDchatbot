#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyjs
#' @import querychat
#' @noRd
app_server <- function(input, output, session) {
  # Reactive values
  selected_dataset <- reactiveVal(NULL)
  dataset_id <- reactiveVal(NULL)

  # Initialize chat module
  chat_module <- querychat_server_debug3(
    id = "chat",
    querychat_config =create_basic_querychat_config(),
    dataset_id = dataset_id
  )

  # Search module
  search_result <- mod_search_server("search")

  # Handle dataset selection
  observeEvent(search_result$selected_dataset(), {
    req(search_result$selected_dataset())

    bslib::toggle_sidebar(id = "main_sidebar")

    selected_dataset(search_result$selected_dataset()$title)
    dataset_id(search_result$selected_dataset()$uid)

    show_dataset_view()
  })

  # Dataset view module
  print(chat_module)
  mod_dataset_view_server("dataset",
                          chat_module = chat_module,
                          selected_dataset = selected_dataset,
                          dataset_id = dataset_id)

  # SQL query module
  mod_sql_query_server("sql_query",
                       chat_module = chat_module)

  # Back button functionality
  observeEvent(input$collapse_btn, {
    show_search_view()
  })

  # UI state management functions
  show_dataset_view <- function() {
    shinyjs::hide("search_ui")
    shinyjs::show("dataset_table")
    shinyjs::show("collapse_btn")
    shinyjs::show("ai_ui")
  }

  show_search_view <- function() {
    bslib::toggle_sidebar(id = "main_sidebar")
    hide("dataset_table")
    shinyjs::show("search_ui")
    shinyjs::hide("collapse_btn")
    shinyjs::hide("ai_ui")
    dataset_id(NULL)
  }
}
