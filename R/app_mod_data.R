# Shiny module: Data upload & column browser
# Part of seminr_app()

#' Data Module UI
#' @param id Module namespace id
#' @keywords internal
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::h4("Load Data"),
          shiny::selectInput(ns("builtin_data"), "Built-in Datasets",
            choices = c("(none)" = "", "mobi", "corp_rep_data"),
            selected = ""
          ),
          shiny::hr(),
          shiny::fileInput(ns("file_upload"), "Upload CSV or RDS",
            accept = c(".csv", ".rds")
          )
        )
      ),
      shiny::column(8,
        shiny::h4("Data Preview"),
        shiny::verbatimTextOutput(ns("data_summary")),
        shiny::tableOutput(ns("data_head"))
      )
    ),
    shiny::fluidRow(
      shiny::column(12,
        shiny::h4("Available Columns"),
        shiny::verbatimTextOutput(ns("column_info"))
      )
    )
  )
}

#' Data Module Server
#' @param id Module namespace id
#' @param preloaded_data Optional reactive data.frame to preload
#' @return A reactive containing the loaded data.frame
#' @keywords internal
mod_data_server <- function(id, preloaded_data = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    data_rv <- shiny::reactiveVal(NULL)

    # Initialize with preloaded data
    shiny::observe({
      d <- preloaded_data()
      if (!is.null(d)) {
        data_rv(d)
      }
    })

    # Load built-in dataset
    shiny::observeEvent(input$builtin_data, {
      if (input$builtin_data != "") {
        d <- get(input$builtin_data, envir = asNamespace("seminr"))
        data_rv(d)
      }
    })

    # Upload file
    shiny::observeEvent(input$file_upload, {
      req_file <- input$file_upload
      ext <- tools::file_ext(req_file$datapath)
      if (ext == "csv") {
        d <- utils::read.csv(req_file$datapath, stringsAsFactors = FALSE)
      } else if (ext == "rds") {
        d <- readRDS(req_file$datapath)
      } else {
        shiny::showNotification("Unsupported file format", type = "error")
        return()
      }
      data_rv(d)
    })

    # Data summary
    output$data_summary <- shiny::renderPrint({
      d <- data_rv()
      if (is.null(d)) {
        cat("No data loaded.")
      } else {
        cat(nrow(d), "rows x", ncol(d), "columns\n")
        str(d, list.len = 20, give.attr = FALSE)
      }
    })

    # Data head
    output$data_head <- shiny::renderTable({
      d <- data_rv()
      shiny::req(d)
      utils::head(d, 10)
    })

    # Column info
    output$column_info <- shiny::renderPrint({
      d <- data_rv()
      if (is.null(d)) {
        cat("Load data to see available columns.")
      } else {
        col_types <- sapply(d, class)
        for (i in seq_along(col_types)) {
          cat(sprintf("  %-25s %s\n", names(col_types)[i], col_types[i]))
        }
      }
    })

    data_rv
  })
}
