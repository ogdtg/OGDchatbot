#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyjs
#' @import waiter
#' @import querychat
#' @noRd
app_ui <- function(request) {
  ui <- tagList(
    add_top_stripes(),
    waiter::use_waiter(),
    page_sidebar(
      title = create_app_title(),
      theme = create_ktg_theme(),
      sidebar = sidebar(
        width = 400,
        height = "100%",
        id = "main_sidebar",
        querychat_ui2("chat-chat"),
        open = FALSE,
        collapsible = TRUE
      ),
      .open = FALSE,
      collapsible = FALSE,
      useShinyjs(),
      add_custom_styles(),

      # Back button
      shinyjs::hidden(actionButton("collapse_btn", "Zurück zur Datenauswahl", class = "btn-tg")),

      # Search module
      mod_search_ui("search"),

      # Dataset view and SQL query modules
      shinyjs::hidden(div(id = "ai_ui",
                          mod_dataset_view_ui("dataset"),
                          mod_sql_query_ui("sql_query")
      ))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "OGDchatbot"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
