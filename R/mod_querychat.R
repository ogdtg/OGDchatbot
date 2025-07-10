create_basic_querychat_config <- function(model =  "gpt-4.1-nano"){
  querychat_init(create_chat_func = purrr::partial(ellmer::chat_openai, model = model),
                 df = mtcars,
                 extra_instructions = "Answer always in German. DO not answer any question that you cannot answer from the data description or with a SQL Stement. DO not give any evaluation that is not based on the results of the SQL.",
                 greeting = '<b>👋 Willkommen beim Thurgauer Daten-Chatbot!</b>

Ich helfe dir, gezielt Informationen aus den offenen Daten von data.tg.ch zu finden – etwa durch Filtern, Sortieren oder statistische Auswertungen.

🧠 <b>Wichtig</b>:
Ich beantworte ausschließlich Fragen, die sich mit nachvollziehbaren SQL-Abfragen lösen lassen.
Das bedeutet: keine Halluzinationen, keine Vermutungen – jede Antwort basiert auf einem transparenten SQL-Statement, das du einsehen kannst.
Denoch ist zu beachten, dass ich nur eine KI bin und Fragen oder Ergebnisse möglicherweise falsch interpretiere. Daher solltest du meine Antworten stets überprüfen.

🔍 Beispiele:

<span class="suggestion">Zeige nur Daten aus dem Jahr 2023</span>


ℹ️ Bitte formuliere deine Fragen möglichst klar und konkret – so kann ich dir am besten helfen.',

  )
}




#' Display a chat greeting in the chat UI
#'
#' Shows a preconfigured greeting if available; otherwise prompts the chat model to introduce itself.
#'
#' @param session The current Shiny session
#' @param greeting HTML-formatted string to show in the chat (can be NULL)
#' @param chat A reactive chat object (must return an object with a `stream_async()` method)
#' @importFrom shinychat chat_append
#' @keywords internal
display_greeting <- function() {
  if (!is.null(greeting) && nzchar(greeting)) {
    shinychat::chat_append(session$ns("chat"), greeting)
  } else {
    shinychat::chat_append(session$ns("chat"), chat()$stream_async("Please introduce yourself."))
  }
  session$sendCustomMessage("scroll_chat_to_bottom", list())
}


#' QueryChat Shiny Server Module (Debug Version)
#'
#' Initializes and manages the reactive state, database connection, and chat tools for the QueryChat module.
#'
#' @param id The module namespace ID
#' @param querychat_config A configuration object created with `querychat::querychat_init`
#' @param dataset_id A reactive expression returning the dataset ID string
#'
#' @return A list of reactive values: `chat`, `df`, `sql`, `title`, and `conn`
#'
#' @import shiny
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom stringr str_replace_all
#' @importFrom shinychat chat_clear chat_append
querychat_server_debug3 <- function(id, querychat_config, dataset_id) {
  shiny::moduleServer(id, function(input, output, session) {

    current_title <- reactiveVal(dataset_id)
    current_query <- reactiveVal("")
    df <- reactiveVal()
    used_tokens <- reactiveVal(0)
    conn <- reactive(DBI::dbConnect(duckdb::duckdb(), dbdir = "/home/dslor@tg.ch/data_parquet/thurgau.duckdb"))
    chat <- reactiveVal()
    system_prompt <- reactiveVal(querychat::querychat_system_prompt(df = df, table_name = deparse(substitute(df)), prompt_template = "prompt_template.md"))

    greeting <- querychat_config[["greeting"]]
    create_chat_func <- querychat_config[["create_chat_func"]]


    # === Append output to chat ===
    append_output <- function(...) {
      txt <- paste0(...)
      shinychat::chat_append_message(
        session$ns("chat"),
        list(role = "assistant", content = txt),
        chunk = TRUE,
        operation = "append",
        session = session
      )
      session$sendCustomMessage("scroll_chat_to_bottom", list())
    }

    # === Tool: Update Dashboard View ===
    update_dashboard <- function(query, title) {
      append_output("\n```sql\n", query, "\n```\n\n")
      tryCatch({
        DBI::dbGetQuery(conn(), query)
      }, error = function(e) {
        append_output("> Error: ", conditionMessage(e), "\n\n")
        stop(e)
      })
      current_query(query)
      current_title(title)
    }

    # === Tool: Run SQL query and return JSON ===
    query <- function(query) {
      cat("query called with:", query, "\n")
      append_output("\n```sql\n", query, "\n```\n\n")
      tryCatch({
        df_result <- DBI::dbGetQuery(conn(), query)
      }, error = function(e) {
        append_output("> Error: ", conditionMessage(e), "\n\n")
        stop(e)
      })
      tbl_html <- querychat:::df_to_html(df_result, maxrows = 5)
      append_output(tbl_html, "\n\n")
      jsonlite::toJSON(df_result, auto_unbox = TRUE)
    }

    # === Chat Setup ===
    initialize_chat <- function(system_prompt) {
      new_chat <- create_chat_func(system_prompt = system_prompt)
      new_chat$register_tool(ellmer::tool(update_dashboard,
                                          "Modifies the data presented in the data dashboard, based on the given SQL query, and also updates the title.",
                                          query = ellmer::type_string("A DuckDB SQL query; must be a SELECT statement."),
                                          title = ellmer::type_string("A title to display at the top of the data dashboard, summarizing the intent of the SQL query.")))
      new_chat$register_tool(ellmer::tool(query,
                                          "Perform a SQL query on the data, and return the results as JSON.",
                                          query = ellmer::type_string("A DuckDB SQL query; must be a SELECT statement.")))

      return(new_chat)
    }

    # === Show greeting ===
    display_greeting <- function() {
      if (!is.null(greeting) && nzchar(greeting)) {
        shinychat::chat_append(session$ns("chat"), greeting)
      } else {
        shinychat::chat_append(session$ns("chat"), chat()$stream_async("Please introduce yourself."))
      }
      session$sendCustomMessage("scroll_chat_to_bottom", list())
    }
    # === Load system prompt and clear chat ===
    observeEvent(dataset_id(), {
      req(dataset_id())
      current_id <- dataset_id()
      table_id <- stringr::str_replace_all(dataset_id(),"-","_")
      cat("Using DuckDB table:", table_id, "\n")

      # Load metadata and generate prompt
      generate_markdown_from_metadata(dataset_id = current_id)


      # Preview data
      preview_df <- tryCatch({
        temp_res <- DBI::dbGetQuery(conn(), sprintf("SELECT * FROM \"%s\"", table_id))
        temp_res
      }, error = function(e) {
        NULL
      })
      df(preview_df)

      prompt <- querychat_system_prompt(
        df = head(df(),5), table_name = table_id,
        data_description = "metadata.md"
      )
      # cat(prompt)
      system_prompt(prompt)

      # Clear chat, reset state
      shinychat::chat_clear(session$ns("chat"))
      chat(initialize_chat(prompt))
      display_greeting()



      current_query("")
      current_title(NULL)
    })

    # === Filtered DataFrame ===
    filtered_df <- reactive({
      query <- current_query()

      if (!is.null(query) && nzchar(query)) {
        tryCatch({
          temp <- DBI::dbGetQuery(conn(), query)
          print(head(temp))  # Print first few rows for debugging
          temp
        }, error = function(e) {
          warning("Query failed: ", conditionMessage(e))
          data.frame()
        })
      } else {
        req(df())
      }
    })


    # === Handle user input ===
    observeEvent(input[["chat-chat_user_input"]], {
      msg <- input[["chat-chat_user_input"]]

      tokens <- chat()$get_tokens()
      used_tokens(used_tokens()+sum(tokens$tokens))

      print(used_tokens())

      if (is.null(dataset_id())) {
        append_output("Bitte erst Datensatz wählen.")
      } else if (used_tokens()>=60000) {
        shinychat::chat_append(session$ns("chat"),paste0("Du hast deine Tokens aufgebraucht."))

      } else {
        shinychat::chat_append(session$ns("chat"), chat()$stream_async(msg))
        session$sendCustomMessage("scroll_chat_to_bottom", list())
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # === Return interface ===

    list(
      chat = reactive(chat()),
      df = filtered_df,
      sql = reactive(current_query()),
      title = reactive(current_title()),
      conn  = reactive(conn())
    )
  })
}


#' QueryChat Sidebar Wrapper
#'
#' Wraps the querychat UI into a collapsible sidebar
#'
#' @param id Module ID for namespace
#' @param width Width of the sidebar
#' @param height Height of the sidebar
#' @param ... Additional arguments passed to `querychat_ui2()`
#'
#' @importFrom bslib sidebar
#' @keywords internal
querychat_sidebar2 <- function(id, width = 400, height = "100%", ...) {
  bslib::sidebar(width = width, height = height, ..., querychat_ui2(id))
}


#' QueryChat UI Module
#'
#' Renders the chat interface and applies styling
#'
#' @param id Module ID for namespace
#'
#' @importFrom shiny NS includeCSS
#' @importFrom htmltools tagList
#' @importFrom shinychat chat_ui
#' @keywords internal
querychat_ui2 <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::includeCSS(system.file("www", "styles.css", package = "querychat")),
    div(
      id = ns("chat_container"),
      style = "height: 100%; display: flex; flex-direction: column;",
      div(
        style = "flex-grow: 1; overflow: auto;",
        shinychat::chat_ui(ns("chat"), height = "100%", fill = TRUE)
      )
    )
  )
}

