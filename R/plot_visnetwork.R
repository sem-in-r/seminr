# Purpose: visNetwork interactive plotting backend for SEMinR models
#
# Provides vis_graph() S3 generic + methods, parallel to dot_graph().
# Integration with plot() via backend = "visnetwork" parameter.
#
# See also: plot_visnetwork_utils.R, plot_dot.R, theme.R

# -- S3 generic --

#' Create an interactive visNetwork plot of a SEMinR model
#'
#' Generates an interactive network visualization using the visNetwork package.
#' This is an alternative to the static DiagrammeR/DOT plots produced by
#' \code{\link{dot_graph}}.
#'
#' @param model A SEMinR model object
#' @param title An optional title for the plot
#' @param theme Theme created with \code{\link{seminr_theme_create}}.
#'   If NULL, the current theme from \code{\link{seminr_theme_get}} is used.
#' @param ... Additional parameters passed to methods
#'
#' @return A \code{visNetwork} htmlwidget object
#' @export
#'
#' @examples
#' \dontrun{
#' mobi_pls <- estimate_pls(data = mobi,
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm)
#' vis_graph(mobi_pls)
#' }
vis_graph <- function(model, title = "", theme = NULL, ...) {
  UseMethod("vis_graph")
}

#' @rdname vis_graph
#' @export
vis_graph.default <- function(model, ...) {
  stop("vis_graph() does not support objects of class '",
       paste(class(model), collapse = "', '"), "'.",
       call. = FALSE)
}

# -- PLS model method (main implementation) --

#' @rdname vis_graph
#' @param measurement_only Plot only measurement part
#' @param structure_only Plot only structure part
#' @export
vis_graph.pls_model <- function(model,
                                title = "",
                                theme = NULL,
                                measurement_only = FALSE,
                                structure_only = FALSE,
                                ...) {

  query_install("visNetwork", "Install visNetwork for interactive plots.")

  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("The 'visNetwork' package is required for interactive plots. ",
         "Install it with: install.packages('visNetwork')", call. = FALSE)
  }

  thm <- theme %||% seminr_theme_get()
  if (inherits(thm, "function")) thm <- thm()

  # Convert model to nodes/edges data.frames

  visdata <- model_to_visdata(model, thm,
                              measurement_only = measurement_only,
                              structure_only = structure_only)

  # Build visNetwork widget
  vis <- visNetwork::visNetwork(
    nodes = visdata$nodes,
    edges = visdata$edges,
    main  = list(text = title, style = paste0(
      "font-family:", thm$plot.fontname,
      ";font-size:", thm$plot.title.fontsize, "px",
      ";color:", thm$plot.title.fontcolor
    )),
    background = thm$plot.bgcolor
  )

  # Configure layout
  has_levels <- any(!is.na(visdata$nodes$level))
  if (has_levels) {
    vis <- visNetwork::visHierarchicalLayout(
      vis,
      direction = "LR",
      sortMethod = "directed",
      levelSeparation = 200,
      nodeSpacing = 120
    )
  } else {
    vis <- visNetwork::visPhysics(vis, enabled = FALSE)
  }

  # Configure interaction
  vis <- visNetwork::visInteraction(
    vis,
    hover = TRUE,
    tooltipDelay = 100,
    tooltipStyle = "position: fixed; visibility: hidden; padding: 8px; font-family: sans-serif; font-size: 12px; background-color: white; border: 1px solid #ccc; border-radius: 4px; box-shadow: 2px 2px 6px rgba(0,0,0,0.2);"
  )

  # Configure node selection
  vis <- visNetwork::visOptions(
    vis,
    highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
    nodesIdSelection = FALSE
  )

  # Group styling
  vis <- visNetwork::visGroups(vis, groupname = "construct",
                               shape = "ellipse")
  vis <- visNetwork::visGroups(vis, groupname = "item",
                               shape = "box")

  # Legend
  vis <- visNetwork::visLegend(
    vis,
    addNodes = list(
      list(label = "Construct", shape = "ellipse",
           color = list(background = thm$sm.node.fill, border = thm$sm.node.color)),
      list(label = "Item", shape = "box",
           color = list(background = thm$mm.node.fill, border = thm$mm.node.color))
    ),
    useGroups = FALSE,
    position = "right",
    width = 0.1
  )

  vis
}

# -- Bootstrap model method (delegates to PLS) --

#' @rdname vis_graph
#' @export
vis_graph.boot_seminr_model <- function(model,
                                        title = "",
                                        theme = NULL,
                                        measurement_only = FALSE,
                                        structure_only = FALSE,
                                        ...) {
  vis_graph.pls_model(model, title, theme, measurement_only, structure_only, ...)
}

# -- Specified (unestimated) model method --

#' @rdname vis_graph
#' @export
vis_graph.specified_model <- function(model,
                                      title = "",
                                      theme = NULL,
                                      measurement_only = FALSE,
                                      structure_only = FALSE,
                                      ...) {

  thm <- theme %||% seminr_theme_get()
  if (inherits(thm, "function")) thm <- thm()

  # Build artificial pls_model (same strategy as dot_graph.specified_model)
  measurement_model <- model$measurement_model
  mm <- mm2matrix(measurement_model)
  mmodel <- as.data.frame(mm)
  sm_constructs <- unique(mmodel$construct)

  weight_matrix <- matrix(1,
    ncol = length(sm_constructs),
    nrow = length(unique(mmodel$measurement)),
    dimnames = list(unique(mmodel$measurement), sm_constructs)
  )

  path_matrix <- matrix(1,
    ncol = length(sm_constructs),
    nrow = length(sm_constructs),
    dimnames = list(sm_constructs, sm_constructs)
  )

  a_model <- list(
    measurement_model = measurement_model,
    mmMatrix       = mm,
    smMatrix       = model$structural_model,
    outer_weights  = weight_matrix,
    outer_loadings = weight_matrix,
    path_coef      = path_matrix,
    constructs     = sm_constructs,
    mmVariables    = unique(mmodel$measurement)
  )
  class(a_model) <- "pls_model"

  # Suppress labels for artificial model
  thm$sm.edge.width_multiplier <- 1
  thm$sm.edge.label.show <- FALSE
  thm$mm.edge.width_multiplier <- 1
  thm$mm.edge.label.show <- FALSE

  vis_graph.pls_model(a_model, title, thm, measurement_only, structure_only, ...)
}

# -- Measurement model only --

#' @rdname vis_graph
#' @export
vis_graph.measurement_model <- function(model, title = "", theme = NULL, ...) {

  thm <- theme %||% seminr_theme_get()
  if (inherits(thm, "function")) thm <- thm()

  # Build artificial model (same as dot_graph.measurement_model)
  mm <- mm2matrix(model)
  mmodel <- as.data.frame(mm)

  a_model <- list(
    measurement_model = model,
    mmMatrix       = mm,
    smMatrix       = matrix(rep(unique(mmodel$construct), 2),
                            ncol = 2,
                            nrow = length(unique(mmodel$construct)),
                            dimnames = list(NULL, c("source", "target"))),
    outer_weights  = matrix(1,
                            ncol = length(unique(mmodel$construct)),
                            nrow = length(unique(mmodel$measurement)),
                            dimnames = list(unique(mmodel$measurement),
                                            unique(mmodel$construct))),
    outer_loadings = matrix(1,
                            ncol = length(unique(mmodel$construct)),
                            nrow = length(unique(mmodel$measurement)),
                            dimnames = list(unique(mmodel$measurement),
                                            unique(mmodel$construct))),
    constructs     = unique(mmodel$construct),
    mmVariables    = unique(mmodel$measurement)
  )
  class(a_model) <- "pls_model"

  thm$mm.edge.width_multiplier <- 1
  thm$mm.edge.label.show <- FALSE

  vis_graph.pls_model(a_model, title, thm, measurement_only = TRUE, ...)
}

# -- Structural model only --

#' @rdname vis_graph
#' @export
vis_graph.structural_model <- function(model, title = "", theme = NULL, ...) {

  thm <- theme %||% seminr_theme_get()
  if (inherits(thm, "function")) thm <- thm()

  sm_constructs <- construct_names(model)
  mm_list <- list()
  for (nm in sm_constructs) {
    mm_list[[nm]] <- reflective(nm, paste0(nm, "_dummy"))
  }
  measurement_model <- do.call(constructs, mm_list)
  mm <- mm2matrix(measurement_model)
  mmodel <- as.data.frame(mm)

  a_model <- list(
    measurement_model = measurement_model,
    mmMatrix       = matrix(),
    smMatrix       = model,
    outer_weights  = matrix(1,
                            ncol = length(unique(mmodel$construct)),
                            nrow = length(unique(mmodel$measurement)),
                            dimnames = list(unique(mmodel$measurement),
                                            unique(mmodel$construct))),
    path_coef      = matrix(1,
                            ncol = length(sm_constructs),
                            nrow = length(sm_constructs),
                            dimnames = list(sm_constructs, sm_constructs)),
    constructs     = unique(mmodel$construct),
    mmVariables    = unique(mmodel$measurement)
  )
  class(a_model) <- "pls_model"

  thm$sm.edge.width_multiplier <- 1
  thm$sm.edge.label.show <- FALSE

  vis_graph.pls_model(a_model, title, thm, structure_only = TRUE, ...)
}

# -- Integration with plot.seminr_model --
# The plot.seminr_model function in plot_dot.R is updated to support
# backend = "visnetwork". See the modified plot.seminr_model below.
