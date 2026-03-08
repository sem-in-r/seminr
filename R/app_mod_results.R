# Shiny module: Results display & interactive plot
# Part of seminr_app()

#' Results Module UI
#' @param id Module namespace id
#' @keywords internal
mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(12,
        shiny::h4("Interactive Model Plot"),
        shiny::uiOutput(ns("plot_container"))
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::h4("Selection Details"),
          shiny::verbatimTextOutput(ns("selection_info"))
        )
      ),
      shiny::column(8,
        shiny::h4("Results Tables"),
        shiny::selectInput(ns("table_choice"), "Show",
          choices = c("Path Coefficients" = "paths",
                      "Loadings" = "loadings",
                      "Reliability" = "reliability",
                      "HTMT" = "htmt",
                      "R-squared" = "rsquared",
                      "f-squared" = "fsquared",
                      "VIF" = "vif")
        ),
        shiny::tableOutput(ns("results_table"))
      )
    )
  )
}

#' Results Module Server
#' @param id Module namespace id
#' @param estimation_reactive Reactive estimation results from estimate module
#' @keywords internal
mod_results_server <- function(id, estimation_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get the best available model (bootstrap > pls)
    current_model <- shiny::reactive({
      est <- estimation_reactive()
      if (is.null(est)) return(NULL)
      if (!is.null(est$boot_model)) return(est$boot_model)
      est$model
    })

    # Plot container
    output$plot_container <- shiny::renderUI({
      model <- current_model()
      if (is.null(model)) {
        return(shiny::div(
          style = "border: 2px dashed #ccc; padding: 60px; text-align: center; color: #999;",
          shiny::h3("Estimate a model to see the interactive plot")
        ))
      }

      if (requireNamespace("visNetwork", quietly = TRUE)) {
        visNetwork::visNetworkOutput(ns("vis_plot"), height = "500px")
      } else {
        shiny::helpText("Install the visNetwork package for interactive plots.")
      }
    })

    # Render visNetwork plot
    output$vis_plot <- visNetwork::renderVisNetwork({
      model <- current_model()
      shiny::req(model)
      vis_graph(model)
    })

    # Selection details (on node/edge click)
    output$selection_info <- shiny::renderPrint({
      model <- current_model()
      if (is.null(model)) {
        cat("No model estimated.")
        return()
      }
      cat("Click on nodes or edges in the plot above to see details.\n\n")

      # Show basic model info
      if (inherits(model, "boot_seminr_model")) {
        cat("Bootstrapped model\n")
      } else if (inherits(model, "pls_model")) {
        cat("PLS model\n")
      }

      cat("Constructs:", paste(model$constructs, collapse = ", "), "\n")
      if (!is.null(model$rSquared)) {
        cat("\nR-squared:\n")
        print(round(model$rSquared, 3))
      }
    })

    # Results tables
    output$results_table <- shiny::renderTable({
      model <- current_model()
      shiny::req(model)

      smry <- summary(model)
      choice <- input$table_choice

      tbl <- switch(choice,
        "paths" = {
          if (inherits(model, "boot_seminr_model")) {
            as.data.frame(smry$bootstrapped_paths)
          } else {
            as.data.frame(smry$paths)
          }
        },
        "loadings" = {
          if (inherits(model, "boot_seminr_model")) {
            as.data.frame(smry$bootstrapped_loadings)
          } else {
            as.data.frame(smry$loadings)
          }
        },
        "reliability" = {
          as.data.frame(smry$reliability)
        },
        "htmt" = {
          if (!is.null(smry$validity$htmt)) {
            as.data.frame(smry$validity$htmt)
          } else {
            data.frame(Message = "HTMT not available for this model.")
          }
        },
        "rsquared" = {
          if (!is.null(model$rSquared)) {
            as.data.frame(model$rSquared)
          } else {
            data.frame(Message = "R-squared not available.")
          }
        },
        "fsquared" = {
          tryCatch(
            as.data.frame(smry$fSquare),
            error = function(e) data.frame(Message = "f-squared not available.")
          )
        },
        "vif" = {
          tryCatch(
            as.data.frame(smry$vif_antecedents),
            error = function(e) data.frame(Message = "VIF not available.")
          )
        },
        data.frame(Message = "Select a table to display.")
      )

      tbl
    }, rownames = TRUE, digits = 4)
  })
}
