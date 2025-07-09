# ===================================================================
# === R/utils_ui.R ===
# ===================================================================

#' Add Top Stripes to UI
#'
#' Creates the decorative top stripes for the application
#'
#' @return Shiny UI element
#' @import shiny
#' @export
add_top_stripes <- function() {
  div(class = "ktg-top-stripes",
      div(class = "stripe stripe-1"),
      div(class = "stripe stripe-2"),
      div(class = "stripe stripe-3"),
      div(class = "stripe stripe-4"),
      div(class = "stripe stripe-5")
  )
}

#' Create App Title
#'
#' Creates the application title with logo
#'
#' @return Shiny UI element
#' @import shiny
#' @export
create_app_title <- function() {
  tags$img(
    src = "https://www.tg.ch/public/upload/assets/20/logo-kanton-thurgau.svg",
    height = "60px",
    style = "margin-left: 10px;"
  )
}

#' Add Custom Styles
#'
#' Adds custom CSS styles to the application
#'
#' @return Shiny UI element
#' @import shiny
#' @import shinyjs
#' @export
add_custom_styles <- function() {
  tags$head(
    useShinyjs(),
    tags$style(HTML("
      .card-container {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        justify-content: center;
        padding: 20px;
      }
      .dataset-card {
        width: 250px;
        border: 1px solid #ddd;
        border-radius: 10px;
        padding: 15px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        background-color: #fff;
        opacity: 0;
        transform: translateY(20px);
        animation: fadeInUp 0.4s forwards;
        cursor: pointer;
      }
      @keyframes fadeInUp {
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
    "))
  )
}

#' Create Disclaimer Text
#'
#' Creates the disclaimer text for the application
#'
#' @return Shiny UI element
#' @import shiny
#' @export
create_disclaimer_text <- function() {
  HTML(
    '<b style="margin-bottom: 1.5rem; max-width: 600px; display: block;">
    Es handelt sich bei diesem Produkt um eine experimentelle Demoversion. Daher kann es unter Umständen zu Wartezeiten bei der Ausführung des Chats kommen.
    <br><br>
    Fragen, Kritik oder Anregungen gerne an
    <a href="mailto:ogd@tg.ch">ogd@tg.ch</a>.
    </b>'
  )
}

#' Create No Results UI
#'
#' Creates UI for when no search results are found
#'
#' @return Shiny UI element
#' @import shiny
#' @export
create_no_results_ui <- function() {
  div(
    style = "text-align: center; padding: 40px;",
    h3("Keine Ergebnisse gefunden"),
    p("Versuchen Sie es mit anderen Suchbegriffen.")
  )
}

#' Create Results Cards
#'
#' Creates cards for search results display
#'
#' @param search_results_df Data frame with search results
#' @param ns Namespace function
#'
#' @return Shiny UI element
#' @import shiny
#' @import jsonlite
#' @export
create_results_cards <- function(search_results_df, ns) {
  cards <- lapply(seq_along(search_results_df$uid), function(i) {
    delay <- (i - 1) * 0.1

    uid_json <- jsonlite::toJSON(search_results_df$uid[i], auto_unbox = TRUE)
    title_json <- jsonlite::toJSON(search_results_df$title[i], auto_unbox = TRUE)

    div(
      class = "dataset-card",
      style = paste0("animation-delay:", delay, "s"),
      tags$h4(search_results_df$title[i]),
      tags$p(search_results_df$uid[i]),
      onclick = sprintf(
        "Shiny.setInputValue('%s', {uid: %s, title: %s}, {priority: 'event'});",
        ns("card_clicked"), uid_json, title_json
      )
    )
  })

  div(id = "div_results_ui", class = "card-container", cards)
}

#' Create Download Full Data Button
#'
#' Creates a download button for the full dataset
#'
#' @param dataset_id Dataset ID
#'
#' @return Shiny UI element
#' @import shiny
#' @export
create_download_full_data_button <- function(dataset_id) {
  tags$a(
    href = paste0("https://data.tg.ch/api/explore/v2.1/catalog/datasets/", dataset_id, "/exports/xlsx?lang=de&timezone=Europe%2FZurich&use_labels=true"),
    target = "_blank",
    class = "btn btn-tg",
    "Download Gesamtdatensatz"
  )
}
