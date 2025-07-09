# ===================================================================
# === R/mod_dataset_view.R ===
# ===================================================================

#' Dataset View Module UI
#'
#' Creates the UI for displaying dataset information and data table
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
#' @import shiny
#' @import DT
#' @export
mod_dataset_view_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("dataset_table_heading")),
    uiOutput(ns("dataset_table_subheading")),
    DTOutput(ns("dataset_table")),
    uiOutput(ns("download_full_data")),
    uiOutput(ns("download_filtered_data"))
  )
}

#' Dataset View Module Server
#'
#' Server logic for displaying dataset information and handling downloads
#'
#' @param id Module namespace ID
#' @param chat_module Chat module object
#' @param selected_dataset Reactive value with selected dataset title
#' @param dataset_id Reactive value with dataset ID
#'
#' @import shiny
#' @import DT
#' @import writexl
#' @export
mod_dataset_view_server <- function(id, chat_module, selected_dataset, dataset_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dataset table heading
    output$dataset_table_heading <- renderUI({
      req(selected_dataset())
      h1(id = "df_heading", tags$b(selected_dataset()))
    })

    # Dataset table subheading
    output$dataset_table_subheading <- renderUI({
      req(chat_module$title())
      h3(id = "df_subheading", chat_module$title())
    })

    # Dataset table
    output$dataset_table <- renderDT(chat_module$df())

    # Download buttons
    output$download_full_data <- renderUI({
      req(dataset_id())
      create_download_full_data_button(dataset_id())
    })

    output$download_filtered_data <- renderUI({
      req(chat_module$df())
      if (nrow(chat_module$df()) == 0) return(NULL)

      downloadButton(ns("download_excel"), "Download aktuelle Ansicht", class = "btn btn-tg")
    })

    # Download handler for filtered data
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("OGD_Chatbot_Daten_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        df <- chat_module$df()
        if (!is.null(df) && nrow(df) > 0) {
          writexl::write_xlsx(df, file)
        } else {
          showNotification("Keine Daten zum Exportieren vorhanden.", type = "error")
        }
      }
    )
  })
}
