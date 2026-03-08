# Shiny module: Estimation controls & execution
# Part of seminr_app()

#' Estimate Module UI
#' @param id Module namespace id
#' @keywords internal
mod_estimate_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::h4("Estimation Settings"),
          shiny::selectInput(ns("method"), "Method",
            choices = c("PLS-PM" = "pls", "CB-SEM" = "cbsem")
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("method"), "'] == 'pls'"),
            shiny::selectInput(ns("inner_weights"), "Inner Weighting",
              choices = c("Path Weighting" = "path", "Factor Weighting" = "factor")
            ),
            shiny::numericInput(ns("max_iter"), "Max Iterations", value = 300, min = 1)
          ),
          shiny::actionButton(ns("run_estimate"), "Run Estimation",
                              class = "btn-success btn-block"),
          shiny::hr(),
          shiny::h4("Bootstrap Settings"),
          shiny::numericInput(ns("nboot"), "Bootstrap Samples", value = 1000, min = 100),
          shiny::numericInput(ns("cores"), "Cores",
                              value = max(1, parallel::detectCores() - 1), min = 1),
          shiny::numericInput(ns("seed"), "Random Seed", value = 123),
          shiny::actionButton(ns("run_bootstrap"), "Run Bootstrap",
                              class = "btn-info btn-block")
        )
      ),
      shiny::column(8,
        shiny::h4("Estimation Status"),
        shiny::verbatimTextOutput(ns("status")),
        shiny::h4("Model Summary"),
        shiny::verbatimTextOutput(ns("model_summary"))
      )
    )
  )
}

#' Estimate Module Server
#' @param id Module namespace id
#' @param data_reactive Reactive data.frame
#' @param model_spec_reactive Reactive model specification from builder
#' @return A reactive list with model and boot_model
#' @keywords internal
mod_estimate_server <- function(id, data_reactive, model_spec_reactive) {
  shiny::moduleServer(id, function(input, output, session) {

    estimated_model <- shiny::reactiveVal(NULL)
    boot_model <- shiny::reactiveVal(NULL)
    status_msg <- shiny::reactiveVal("Ready. Build a model and click 'Run Estimation'.")

    # Run estimation
    shiny::observeEvent(input$run_estimate, {
      d <- data_reactive()
      spec <- model_spec_reactive()

      if (is.null(d)) {
        status_msg("Error: No data loaded.")
        return()
      }
      if (is.null(spec) || is.null(spec$structural_model)) {
        status_msg("Error: Model specification incomplete. Add constructs and paths.")
        return()
      }

      status_msg("Running estimation...")

      tryCatch({
        if (input$method == "pls") {
          inner_wt <- if (input$inner_weights == "path") path_weighting else path_factorial
          model <- estimate_pls(
            data = d,
            measurement_model = spec$measurement_model,
            structural_model = spec$structural_model,
            inner_weights = inner_wt,
            missing = mean_replacement,
            missing_value = NA
          )
        } else {
          model <- estimate_cbsem(
            data = d,
            measurement_model = spec$measurement_model,
            structural_model = spec$structural_model
          )
        }

        estimated_model(model)
        boot_model(NULL)  # Reset bootstrap when re-estimating
        status_msg("Estimation complete.")
      }, error = function(e) {
        status_msg(paste0("Estimation failed: ", conditionMessage(e)))
      })
    })

    # Run bootstrap
    shiny::observeEvent(input$run_bootstrap, {
      model <- estimated_model()
      if (is.null(model)) {
        status_msg("Error: Run estimation first before bootstrapping.")
        return()
      }
      if (!inherits(model, "pls_model")) {
        status_msg("Bootstrap is only available for PLS models.")
        return()
      }

      status_msg(paste0("Running bootstrap (", input$nboot, " samples)..."))

      tryCatch({
        set.seed(input$seed)
        boot <- bootstrap_model(model,
          nboot = input$nboot,
          cores = input$cores
        )
        boot_model(boot)
        status_msg(paste0("Bootstrap complete (", input$nboot, " samples)."))
      }, error = function(e) {
        status_msg(paste0("Bootstrap failed: ", conditionMessage(e)))
      })
    })

    # Status output
    output$status <- shiny::renderPrint({
      cat(status_msg())
    })

    # Model summary
    output$model_summary <- shiny::renderPrint({
      boot <- boot_model()
      model <- estimated_model()

      if (!is.null(boot)) {
        summary(boot)
      } else if (!is.null(model)) {
        summary(model)
      } else {
        cat("No model estimated yet.")
      }
    })

    # Return reactive results
    shiny::reactive({
      list(
        model      = estimated_model(),
        boot_model = boot_model()
      )
    })
  })
}
