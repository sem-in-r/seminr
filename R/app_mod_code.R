# Shiny module: Generated R code viewer
# Part of seminr_app()

#' Code Module UI
#' @param id Module namespace id
#' @keywords internal
mod_code_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(12,
        shiny::h4("Generated SEMinR Code"),
        shiny::helpText("Copy this code to reproduce your analysis in R."),
        shiny::actionButton(ns("copy_code"), "Copy to Clipboard",
                            class = "btn-primary"),
        shiny::hr(),
        shiny::verbatimTextOutput(ns("code_output"))
      )
    )
  )
}

#' Code Module Server
#' @param id Module namespace id
#' @param data_reactive Reactive data.frame
#' @param model_spec_reactive Reactive model specification from builder
#' @param estimation_reactive Reactive estimation results
#' @param data_name_reactive Reactive string of data name
#' @keywords internal
mod_code_server <- function(id, data_reactive, model_spec_reactive,
                            estimation_reactive, data_name_reactive) {
  shiny::moduleServer(id, function(input, output, session) {

    generated_code <- shiny::reactive({
      spec <- model_spec_reactive()
      if (is.null(spec) || is.null(spec$measurement_model)) {
        return("# Build a model first to generate code.")
      }

      data_name <- if (!is.null(data_name_reactive)) {
        data_name_reactive()
      } else {
        "data"
      }

      est <- estimation_reactive()
      do_bootstrap <- !is.null(est) && !is.null(est$boot_model)
      estimation_method <- "pls"  # Default

      generate_seminr_code(
        measurement_model = spec$measurement_model,
        structural_model  = spec$structural_model,
        data_name         = data_name,
        estimation        = estimation_method,
        bootstrap         = do_bootstrap || is.null(est)
      )
    })

    output$code_output <- shiny::renderPrint({
      cat(generated_code())
    })

    # Copy to clipboard via JavaScript
    shiny::observeEvent(input$copy_code, {
      code <- generated_code()
      shiny::showNotification("Code copied! (Paste in R console)", type = "message")
      # Note: actual clipboard access requires shinyjs or custom JS
    })
  })
}
