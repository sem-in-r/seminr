# Purpose: Main Shiny app launcher for SEMinR interactive workflow
#
# Provides seminr_app() which launches a Shiny application for
# data-driven model building, estimation, and results exploration.
#
# See also: app_modules/mod_data.R, mod_builder.R, mod_estimate.R,
#           mod_results.R, mod_code.R

#' Launch the SEMinR Interactive App
#'
#' Opens a Shiny application for building, estimating, and exploring
#' Structural Equation Models interactively. Supports the full SEMinR
#' workflow: load data, visually specify constructs and paths, run
#' estimation (PLS or CBSEM), bootstrap, and explore results.
#'
#' @param data Optional data.frame to preload into the app
#' @param model Optional estimated SEMinR model to preload for exploration
#' @param port Port for the Shiny app (default: auto-selected)
#' @param launch.browser Whether to open the app in a browser (default: TRUE)
#'
#' @return Runs the Shiny app (does not return until app is closed)
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch empty app
#' seminr_app()
#'
#' # Launch with preloaded data
#' seminr_app(data = mobi)
#'
#' # Launch with estimated model for exploration
#' seminr_app(model = mobi_pls)
#' }
seminr_app <- function(data = NULL, model = NULL, port = NULL,
                       launch.browser = TRUE) {

  # Check required packages
  required_pkgs <- c("shiny", "visNetwork")
  for (pkg in required_pkgs) {
    query_install(pkg, paste0(
      "The seminr_app() requires the '", pkg, "' package. ",
      "Install it with: install.packages('", pkg, "')"
    ))
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Required package '", pkg, "' is not available.", call. = FALSE)
    }
  }

  # Source module files
  # (These are loaded via devtools::load_all() or package loading)

  # Build UI
  ui <- shiny::navbarPage(
    title = "SEMinR",
    theme = if (requireNamespace("bslib", quietly = TRUE)) {
      bslib::bs_theme(bootswatch = "flatly")
    } else {
      NULL
    },

    shiny::tabPanel("Data",     mod_data_ui("data")),
    shiny::tabPanel("Builder",  mod_builder_ui("builder")),
    shiny::tabPanel("Estimate", mod_estimate_ui("estimate")),
    shiny::tabPanel("Results",  mod_results_ui("results")),
    shiny::tabPanel("Code",     mod_code_ui("code"))
  )

  # Build Server
  server <- function(input, output, session) {

    # Data module
    data_rv <- mod_data_server("data",
      preloaded_data = shiny::reactive(data)
    )

    # Builder module
    model_spec <- mod_builder_server("builder", data_rv)

    # If a pre-estimated model is provided, create a reactive for it
    preloaded_estimation <- if (!is.null(model)) {
      shiny::reactive(list(model = model, boot_model = NULL))
    } else {
      NULL
    }

    # Estimate module
    estimation <- mod_estimate_server("estimate", data_rv, model_spec)

    # Use preloaded model if no estimation has been run
    active_estimation <- shiny::reactive({
      est <- estimation()
      if (!is.null(est$model)) {
        est
      } else if (!is.null(preloaded_estimation)) {
        preloaded_estimation()
      } else {
        list(model = NULL, boot_model = NULL)
      }
    })

    # Results module
    mod_results_server("results", active_estimation)

    # Data name for code generation
    data_name <- shiny::reactive({
      # Try to determine data source name
      if (!is.null(data)) {
        deparse(substitute(data, env = parent.frame()))
      } else {
        "data"
      }
    })

    # Code module
    mod_code_server("code", data_rv, model_spec, active_estimation, data_name)
  }

  # Launch
  app_args <- list(ui = ui, server = server)
  if (!is.null(port)) app_args$options <- list(port = port)
  app_args$options <- c(app_args$options, list(launch.browser = launch.browser))

  do.call(shiny::shinyApp, app_args)
}
