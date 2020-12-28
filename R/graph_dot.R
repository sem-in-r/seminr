# TEST
if (FALSE) {
  library(seminr)

  mobi <- mobi

  #seminr syntax for creating measurement model
  mobi_mm <- constructs(reflective("Image",        multi_items("IMAG", 1:5)),
                        reflective("Expectation",  multi_items("CUEX", 1:3)),
                        reflective("Quality",      multi_items("PERQ", 1:7)),
                        reflective("Value",        multi_items("PERV", 1:2)),
                        reflective("Satisfaction", multi_items("CUSA", 1:3)),
                        reflective("Complaints",   single_item("CUSCO")),
                        composite("Loyalty",      multi_items("CUSL", 1:3)))

  #seminr syntax for creating structural model
  mobi_sm <- relationships(paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
                           paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
                           paths(from = "Quality",      to = c("Value", "Satisfaction")),
                           paths(from = "Value",        to = c("Satisfaction")),
                           paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
                           paths(from = "Complaints",   to = "Loyalty"))

  mobi_pls <- estimate_pls(data = mobi,
                           measurement_model = mobi_mm,
                           structural_model = mobi_sm)

  plot_theme <- create_theme(plot.title.fontsize = 40,
                             plot.fontname = "Times",
                             mm.node.fill = "firebrick",
                             sm.node.fill = "pink")

  dot_graph(mobi_pls, theme = plot_theme) %>% DiagrammeR::grViz()
}


globalVariables(c("."))

# Utilities ----

glue_dot <- function(x) {
  glue::glue(x, .open = "<<", .close = ">>", .envir = parent.frame())
}

# TODO: document purpose of this function
#
extract_mm_coding <- function(model) {
  construct_names <- c()
  construct_types <- c()
  for (i in seq_along(model$measurement_model)) {
    c(construct_names, model$measurement_model[[i]][[1]]) -> construct_names
    c(construct_types, names(model$measurement_model)[i]) -> construct_types
  }
  mm_coding <- matrix(nrow = length(construct_names),
                      ncol = 2,
                      data = c(construct_names, construct_types))
  colnames(mm_coding) <- c("name", "type")
  return(mm_coding)
}





#' Convert a seminr measurement model to a Graphviz representation
#'
#' The function also tries to run a CFA on the measurement model and uses the loadings
#' in plots.
#' With the help of the \code{DiagrammeR} package this code can then be plotted in
#' various contexts.
#'
#' Current limitations:
#' - experimental
#'
#' @param measurement_model Model created with \code{seminr}.
#' @param title An optional title for the plot
#' @param theme Theme created with \code{\link{create_theme}}.
#'
#' @return The path model as a formatted string in dot language.
#' @export
#'
#' @examples
#' mobi <- mobi
#'
#' #seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' res <- dot_graph.measurement_model(mobi_mm, title = "Example plot")
#'
#' \dontrun{
#' DiagrammeR::grViz(res)
#' }
dot_graph.measurement_model <- function(measurement_model, title, theme = NULL){
  if (!c("measurement_model" %in% class(measurement_model))) {
    stop(
      paste("This functions plots a measurement model. You supplied a ", paste(class(model), collapse = ",")),
      call. = FALSE
    )
  }

  # THIS IS AN ARTIFICAL MODEL THAT LETS ME REUSE THE OLD PLOTTING FUNCTION,
  # THIS is uneccessary complex.
  mm <- measurement_model %>% mm2matrix()
  mm %>% as.data.frame() -> mmodel
  model <- list(measurement_model = measurement_model,
                mmMatrix = mm,
                outer_weights = matrix(c(1), # add only 1s
                                       ncol = length(mmodel$construct %>% unique()),
                                       dimnames = list(mmodel$measurement %>% unique, mmodel$construct %>% unique()),
                                       nrow = length(mmodel$measurement %>% unique())
                                       ),
                constructs = mmodel$construct %>% unique(),
                mmVariables = mmodel$measurement %>% unique()
  )

  class(model) <- "pls_model"

  dot_graph(model, title = title, theme = theme, measurement_only = TRUE)

}


#' Convert a seminr model to Graphviz representation
#'
#' With the help of the \code{DiagrammeR} package this code can then be plotted in
#' various contexts.
#'
#' Current limitations:
#' - Only plots PLS Models
#' - no higher order constructs
#' - No interaction terms
#'
#' @param model Model created with \code{seminr}.
#' @param title An optional title for the plot
#' @param theme Theme created with \code{\link{create_theme}}.
#' @param measurement_only Plot only measurement part
#' @param structure_only Plot only structure part
#'
#' @return The path model as a formatted string in dot language.
#' @export
#'
#' @examples
#' mobi <- mobi
#'
#' #seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' #seminr syntax for creating structural model
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' mobi_pls <- estimate_pls(data = mobi,
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm)
#'
#' # generate dot-Notation
#' res <- dot_graph(mobi_pls, title = "Example plot")
#'
#' \dontrun{
#' DiagrammeR::grViz(res)
#' }
dot_graph <- function(model, title = "", theme = NULL, measurement_only = FALSE, structure_only = FALSE) {


  # adapt when necessary
  if (!c("pls_model" %in% class(model))) {
    stop(
      paste("Currently only pls_models are supported. You supplied", paste(class(model), collapse = ",")),
      call. = FALSE
    )
  }

  if (is.null(theme)) {
    thm <- seminr_theme_get()
  } else {
    thm <- theme
  }

  thm$plot.title <- title

  global_style <- get_global_style(theme = thm)

  #rewrite construct size
  #c_width_offst <- 0.1
  #if (thm$construct_nodes$shape %in% c("ellipse", "oval")) {
    c_width_offst <- 0.4
  #}
  # HERE ----
  construct_width <- model$constructs %>% graphics::strwidth(.,font = thm$sm.node.label.fontsize, units = "in") %>% max() + c_width_offst
  construct_height <- model$constructs %>% graphics::strheight(.,font = thm$sm.node.label.fontsize, units = "in") %>% max() + c_width_offst

  thm$sm.node.width  <- construct_width
  thm$sm.node.height <- construct_height * 2

  # rewrite item size
  i_width_offst <- 0.1
  #if (thm$item_nodes$shape %in% c("ellipse", "oval")) {
  #  i_width_offst <- 0.4
  #}
  item_width <- model$mmVariables %>% graphics::strwidth(.,font = thm$mm.node.label.fontsize, units = "in") %>% max() + i_width_offst
  item_height <- model$mmVariables %>% graphics::strheight(.,font = thm$mm.node.label.fontsize, units = "in") %>% max() + i_width_offst

  thm$mm.node.width <- item_width
  thm$mm.node.height <- item_height

  mm <- ""
  sm <- ""
  if (!measurement_only) {
    sm <- dot_component_sm(model = model, theme = thm)
  }
  if (!structure_only) {
    mm <- dot_component_mm(model = model, theme = thm)
  }


  glue_dot(paste0("digraph G {\n",
                  "\n<<global_style>>\n",
                  "\n<<sm>>",
                  "\n<<mm>>",
                  "\n}\n"))
}





# GLOBAL ------------------

# get global theme options
get_global_style <- function(theme) {
  glue_dot(paste0("// ----------------------\n",
                  "// General graph settings\n",
                  "// ----------------------\n",
                  "graph [\n",
                  "charset = 'UTF-8',\n",
                  "layout = dot,\n",
                  "label = '<<theme$plot.title>>',\n",
                  "fontsize = <<theme$plot.title.fontsize>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "rankdir = LR,\n",
                  "labelloc = t,\n",
                  "splines = <<theme$plot.splines>>\n",
                  "]\n"))
}


# SM ----------------------

# construct structural model subgraph
dot_component_sm <- function(model, theme) {
  sm_nodes <- extract_sm_nodes(model, theme)
  sm_node_style <- get_sm_node_style(theme)
  sm_edges <- extract_sm_edges(model, theme)
  sm_edge_style <- get_sm_edge_style(theme)
  glue_dot(paste0("// --------------------\n",
                  "// The structural model\n",
                  "// --------------------\n",
                  "subgraph sm {\n",
                  "rankdir = LR;\n",
                  "node [\n",
                  "<<sm_node_style>>\n",
                  "]\n",
                  "<<sm_nodes>>\n",
                  "edge [\n",
                  "<<sm_edge_style>>\n",
                  "]\n",
                  "<<sm_edges>>\n",
                  "}\n"))
}

# extract structural model nodes from a seminr model
extract_sm_nodes <- function(model, theme) {
  sm_nodes <- gsub("\\*", "_x_", model$constructs)
  sm_nodes <- sapply(sm_nodes, format_sm_node, model, theme)
  sm_nodes <- paste0(sm_nodes, collapse = "\n")
  return(sm_nodes)
}

# format structural model node where appropriate
format_sm_node <- function(construct, model, theme){

  # this is the unicode symbol for ^2
  squared_symbol <- "\U00B2"

  formatted_node <- ""
  #TODO: switch to adjusted
  if (construct %in% colnames(model$rSquared)) {
    formatted_node <- paste0(construct,
                             " [label='", construct, "\nr",squared_symbol,"=", round(model$rSquared[1, construct], theme$plot.rounding), "']")
  } else {
    formatted_node <- paste0(construct)
  }
  return(formatted_node)
}

get_sm_node_style <- function(theme) {
  glue_dot(paste0("shape = ellipse,\n",
                  "color = <<theme$sm.node.color>>,\n",
                  "fillcolor = <<theme$sm.node.fill>>,\n",
                  "style = filled,\n",
                  "fontsize = <<theme$sm.node.label.fontsize>>,\n",
                  "height = <<theme$sm.node.height>>,\n",
                  "width = <<theme$sm.node.width>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "fixedsize = true\n"))
}

# extract structural model edges from a seminr model
extract_sm_edges <- function(model, theme, weights = 1) {
  if ("boot_seminr_model" %in% class(model)) {
    cat("Using a bootstrapped PLS model\n")
    model$paths_descriptives
  } else {
    cat("Using an estimated PLS model\n")
  }

  nr <- nrow(model$smMatrix)
  nc <- ncol(model$smMatrix)
  sm <- model$smMatrix
  sm_edges <- c()

  # Unicode for small mathematical symbols
  beta <- "\U0001D6FD"
  #print(beta)
  gamma <- "\U0001D6FE"
  #print(gamma)

  for (i in 1:nrow(sm)) {
    letter <- beta
    if ( !(sm[i,1] %in% colnames(model$rSquared))) {
      letter <- gamma
    }

    coef <- round(model$path_coef[sm[i, 1], sm[i,2]], theme$plot.rounding)
    sm_edges <- c(sm_edges,
                  paste0(sm[i, 1], " -> {", sm[i, 2], "}",
                         "[weight = ", weights, ", label = '",letter," = ", coef, "', penwidth = ", abs(coef * 5),"]"))
  }
  sm_edges <- paste0(sm_edges, collapse = "\n")
  sm_edges <- gsub("\\*", "_x_", sm_edges)
  return(sm_edges)
}

get_sm_edge_style <- function(theme){
  minlen_str <- ""
  if (!is.na(theme$sm.edge.minlen)) {
    minlen_str <- glue_dot("minlen = <<theme$sm.edge.minlen>>,\n")
  }
  glue_dot(paste0("color = <<theme$sm.edge.color>>,\n",
                  "fontsize = <<theme$sm.edge.label.fontsize>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "<<minlen_str>>",
                  "dir = both,\n",
                  "arrowhead = normal,\n",
                  "arrowtail = none"))

}


# MM ----------------------

dot_component_mm <- function(model, theme) {
  sub_components_mm <- c(paste0("// ---------------------\n",
                                "// The measurement model\n",
                                "// ---------------------\n"))

  for (i in 1:length(model$constructs)) {
    sub_component <- dot_subcomponent_mm(i, model, theme)
    sub_components_mm <- c(sub_components_mm, sub_component)
  }

  glue_dot(paste0(sub_components_mm, collapse = "\n"))

}

dot_subcomponent_mm <- function(index, model, theme) {

  node_style <- get_mm_node_style(theme)

  mm_coding <- extract_mm_coding(model)
  is_reflective <- mm_coding[index, 2] == "reflective"
  if (is_reflective) {
    edge_style <- get_mm_edge_style(theme, forward = FALSE)
  } else {
    edge_style <- get_mm_edge_style(theme, forward = TRUE)
  }

  nodes <- extract_mm_nodes(index, model)
  edges <- extract_mm_edges(index, model, theme)

  sub_component <- glue_dot(paste0(c("subgraph construct_<<index>> {",
                                     "node [",
                                     "<<node_style>>",
                                     "]",
                                     "<<nodes>>",
                                     "edge [",
                                     "<<edge_style>>",
                                     "]",
                                     "<<edges>>",
                                     "}\n"),
                                   collapse = "\n"))

  return(sub_component)
}


get_mm_node_style <- function(theme) {
  glue_dot(paste0("shape = box,\n",
                  "color = <<theme$mm.node.color>>,\n",
                  "fillcolor = <<theme$mm.node.fill>>,\n",
                  "style = filled,\n",
                  "fontsize = <<theme$mm.node.label.fontsize>>,\n",
                  "height = <<theme$mm.node.height>>,\n",
                  "width = <<theme$mm.node.width>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "fixedsize = true\n"))
}

get_mm_edge_style <- function(theme, forward){
  if (forward) {
    arrowhead <- "normal"
    arrowtail <- "none"
  } else {
    arrowhead <- "none"
    arrowtail <- "normal"
  }

  if (!is.na(theme$mm.edge.minlen)) {
    minlen_str <- glue_dot("minlen = <<theme$mm.edge.minlen>>,")
  } else {
    minlen_str <- ""
  }

  glue_dot(paste0(c("color = <<theme$mm.edge.color>>,",
                    "fontsize = <<theme$mm.edge.label.fontsize>>,",
                    "fontname = <<theme$plot.fontname>>,",
                    "<<minlen_str>>",
                    "dir = both",
                    "arrowhead = <<arrowhead>>",
                    "arrowtail = <<arrowtail>>"),
                  collapse = "\n"))
}


extract_mm_nodes <- function(index, model) {
  mm_coding <- extract_mm_coding(model)
  mm_matrix <- model$mmMatrix
  mm_matrix_subset <- mm_matrix[mm_matrix[, 1] == mm_coding[index, 1], ]
  if (!is.vector(mm_matrix_subset)) {
    nodes <- paste0(mm_matrix_subset[, 2], collapse = "\n")
  } else {
    nodes <- paste0(mm_matrix_subset[2], collapse = "\n")
  }
  return(nodes)
}


extract_mm_edges <- function(index, model, theme, weights = 1000) {
  mm_coding <- extract_mm_coding(model)
  mm_matrix <- model$mmMatrix
  mm_matrix_subset <- mm_matrix[mm_matrix[, 1] == mm_coding[index, 1], ]
  edges <- ""


  # determine letter to use (What is with A and B type constructs?)
  # Small mathematical lamda
  lamda <- "\U0001D706"
  #print(lamda)


  if (is.vector(mm_matrix_subset)) {
    if (theme$mm.edge.use_outer_weights) {
      loading <- round(model$outer_weights[mm_matrix_subset[2], mm_matrix_subset[1]], theme$plot.rounding)
    } else {
      loading <- round(model$outer_loadings[mm_matrix_subset[2], mm_matrix_subset[1]], theme$plot.rounding)
    }

    if (grepl("\\*", mm_matrix_subset[2])) {
      # show interaction indicators?
    } else {

      #
      letter <- "w"
      if ( mm_matrix_subset[3] == "C") {
        letter <- lamda
      }
      edges <- paste0(edges,
                      mm_matrix_subset[2], " -> {", mm_matrix_subset[1], "}",
                      "[weight = ", weights, ", label = '", letter, " = ", loading ,"', penwidth = ", abs(loading * 3), "]\n")
    }
  } else {# is.matrix() == TRUE
    for (i in 1:nrow(mm_matrix_subset)) {
      if (theme$mm.edge.use_outer_weights) {
        loading <- round(model$outer_weights[mm_matrix_subset[i, 2], mm_matrix_subset[i, 1]], theme$plot.rounding)
      } else {
        loading <- round(model$outer_loadings[mm_matrix_subset[i, 2], mm_matrix_subset[i, 1]], theme$plot.rounding)
      }

      if (grepl("\\*", mm_matrix_subset[i, 2])) {
        # show interaction indicators?
      } else {
        #
        letter <- "w"
        if ( mm_matrix_subset[i,3] == "C") {
          letter <- lamda
        }
        edges <- paste0(edges,
                        mm_matrix_subset[i, 2], " -> {", mm_matrix_subset[i, 1], "}",
                        "[weight = ", weights, ", label = '", letter, " = ", loading ,"', penwidth = ", abs(loading * 3), "]\n")
      }
    }
  }

  edges <- gsub("\\*", "_x_", edges)
  return(edges)
}


