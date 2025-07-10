# ===================================================================
# === R/utils_data.R ===
# ===================================================================

#' Search Datasets
#'
#' Performs vector search on the dataset catalog
#'
#' @param search_term Search term
#'
#' @return Search results
#' @export
search_datasets <- function(search_term) {
  vector_search_ods(search_term)
}

#' Process Search Results
#'
#' Processes and filters search results
#'
#' @param search_results Raw search results
#' @param catalog vector of non-geodata dataset_id
#'
#' @return Processed data frame
#' @import dplyr
#' @export
process_search_results <- function(search_results,catalog) {
  search_results %>%
    dplyr::bind_rows() %>%
    dplyr::filter(uid %in% catalog)
}

#' Execute SQL Query
#'
#' Safely executes a SQL query and returns results
#'
#' @param conn Database connection
#' @param sql_query SQL query string
#'
#' @return List with success flag and data/error message
#' @import DBI
#' @export
execute_sql_query <- function(conn, sql_query) {
  tryCatch({
    df <- DBI::dbGetQuery(conn, sql_query)
    list(success = TRUE, data = df)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}


get_non_geodata <- function(){
  odsAPI::get_catalog("data.tg.ch") %>%
    dplyr::filter(has_records) %>%
    pull(dataset_id)
}


#' Generate Metadata Markdown from ODS Dataset
#'
#' Downloads and formats metadata for a given dataset ID from data.tg.ch into a Markdown document.
#'
#' @param dataset_id The dataset identifier on data.tg.ch
#' @param output_dir The output directory for the markdown file (default = current directory)
#' @return No return value. Writes `metadata.md` to disk.
#' @export
#'
#' @importFrom odsAPI get_metadata
#' @importFrom stringr str_remove_all
#' @importFrom utils write.table
generate_markdown_from_metadata <- function(dataset_id, output_dir = ".") {
  metas <- odsAPI::get_metadata(domain = "data.tg.ch", dataset_id)
  fields <- metas$fields
  md <- c()

  # Title
  title <- metas$default$title
  md <- c(md, paste0("# ", metas$metas$default$title), "")

  # Description
  description <- metas$default$description %>% stringr::str_remove_all("<[^>]+>")
  md <- c(md, "## Beschreibung", description, "")

  # Metadata summary
  md <- c(md, "## Metadaten", "")
  md <- c(md, paste0("- **Herausgeber**: ", metas$metas$default$publisher))
  md <- c(md, paste0("- **Thema**: ", metas$metas$default$theme))
  md <- c(md, paste0("- **Schlagwörter**: ", paste(metas$metas$default$keyword, collapse = ", ")), "")

  # Fields
  md <- c(md, "## Felder", "")
  for (i in seq_len(nrow(fields))) {
    field <- fields[i, ]
    md <- c(md,
            paste0("### `", field$name, "`"),
            paste0("- **Beschreibung**: ", field$description),
            if (identical(field$type, "date") && isTRUE(field$annotations.timeserie_precision == "year")) {
              "- **Typ**: int"
            } else {
              paste0("- **Typ**: ", field$type)
            },
            "")
  }

  markdown_text <- paste(md, collapse = "\n")
  filename <- file.path(output_dir, "metadata.md")
  writeLines(markdown_text, filename)
}





#' Perform Vector Search on Thurgau ODS
#'
#' Executes a similarity-based vector search using the ODS API from data.tg.ch.
#'
#' @param searchterm A search phrase
#' @return A list of dataset metadata results
#' @export
#'
#' @importFrom httr GET content
#' @importFrom glue glue
vector_search_ods <- function(searchterm) {
  url <- glue::glue("https://data.tg.ch/api/explore/v2.1/catalog/datasets/?lang=de&limit=8&order_by=score+DESC&select=%22ods_dataset%22+as+asset_type%2C+%22data%22+as+category%2C+modified+as+updated_at%2C+dataset_id+as+uid%2C+title%2C+description%2C+publisher+as+creator%2C+score%28%29+as+score&where=%28vector_similarity_threshold%28%22{URLencode(searchterm)}%22%29%29")

  res <- httr::GET(url)
  results <- content(res)
  return(results$results)
}



#' Estimate Token Count for Text
#'
#' Approximates the number of tokens (as used by models like GPT) in a given text based on character classes.
#'
#' @param text A character string to analyze
#' @return Integer estimate of token count
#' @export
#'
#' @importFrom stats setNames
estimate_tokens <- function(text) {
  clusters <- list(
    C0 = strsplit("NORabcdefghilnopqrstuvy", "")[[1]],
    C1 = strsplit('"#%)\\*+56789<>?@Z[\\]^|§«äç\'', "")[[1]],
    C2 = strsplit('-.ABDEFGIKWY_\r\tz{ü', "")[[1]],
    C3 = strsplit(',01234:~Üß', "")[[1]],
    C4 = character(0),
    C5 = strsplit('!$&(/;=JX`j\n}ö', "")[[1]],
    C6 = strsplit('CHLMPQSTUVfkmspwx ', "")[[1]]
  )

  avgtokenPerClass <- c(
    C4 = 0.08086208692099685,
    C0 = 0.2020182639633662,
    C6 = 0.2372744211422125,
    C2 = 0.3042805747355606,
    C5 = 0.4157646363858563,
    C1 = 0.4790556468110302,
    C3 = 0.6581971122770317,
    CX = 0.980083857442348
  )

  classify_char <- function(chars, i) {
    ch <- chars[i]
    prev <- if (i > 1) chars[i - 1] else ""

    if (ch == " ") {
      return(if (prev == " ") "C4" else "C0")
    } else if (utf8ToInt(ch) > 255) {
      return("C3")
    } else {
      for (cls in names(clusters)) {
        if (ch %in% clusters[[cls]]) return(cls)
      }
      return("CX")
    }
  }

  chars <- strsplit(text, "")[[1]]
  classes <- vapply(seq_along(chars), function(i) classify_char(chars, i), character(1))
  token_estimate <- sum(avgtokenPerClass[classes])
  round(token_estimate)
}
