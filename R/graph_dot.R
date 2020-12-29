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

#' Wrap a text in single quotes
#'
#' @param x a character string
esc_node <- function(x){
  paste0("'", x ,"'")
}


# DOT GRAPH ----

#' Generate a dot graph from various SEMinR models
#'
#' With the help of the \code{DiagrammeR} package this dot graph can then be plotted in
#' various in RMarkdown, shiny, and other contexts.
#' Depending on the type of model, different parameters can be used.
#'
#' @param model The model description
#' @param title An optional title for the plot
#' @param theme Theme created with \code{\link{seminr_theme_create}}.
#' @param ... Additional parameters
#'
#' @return The path model as a formatted string in dot language.
#' @export
#'
# @examples
dot_graph <- function(model,
                      title = "",
                      theme = NULL,
                      ...) {
  UseMethod("dot_graph")
}

dot_graph.default <- function(x, ...){
  stop("Whoops. This shouldn't have happened. Please let us know if this happens and how.")
}

#' Convert a seminr measurement model to a Graphviz representation
#'
#' With the help of the \code{DiagrammeR} package this code can then be plotted in
#' various contexts.
#'
#'
#' @rdname dot_graph
#' @param model Model created with \code{seminr}.
#' @param title An optional title for the plot
#' @param theme Theme created with \code{\link{seminr_theme_create}}.
#' @param ... Unused
#'
# @return The path model as a formatted string in dot language.
#' @export
#'
#' @examples
#' # - - - - - - - - - - - - - - - -
#' # Example for plotting a measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' res <- dot_graph(mobi_mm, title = "Preview measurement model")
#'
#' \dontrun{
#' DiagrammeR::grViz(res)
#' }
dot_graph.measurement_model <-
  function(model,
           title = "",
           theme = NULL, ...
  ){

    unusedParams <- list(...)
    if (length(unusedParams))
      stop('Unused parameters: ', paste(unusedParams, collapse = ', '))

  if (is.null(theme)) {
      thm <- seminr_theme_get()
  } else {
      thm <- theme
  }

  # THIS IS AN ARTIFICAL MODEL THAT LETS ME REUSE THE OLD PLOTTING FUNCTION,
  # THIS is unnecessary complex(?).
  mm <- model %>% mm2matrix()
  mm %>% as.data.frame() -> mmodel
  a_model <- list(measurement_model = model,
                mmMatrix = mm,
                outer_weights = matrix(c(1), # add only 1s
                                       ncol = length(mmodel$construct %>% unique()),
                                       dimnames = list(mmodel$measurement %>% unique, mmodel$construct %>% unique()),
                                       nrow = length(mmodel$measurement %>% unique())
                                       ),
                constructs = mmodel$construct %>% unique(),
                mmVariables = mmodel$measurement %>% unique()
  )

  class(a_model) <- "pls_model"

  thm$mm.edge.width_multiplier <- 1
  thm$mm.edge.label.show <- FALSE
  dot_graph(a_model, title = title, theme = thm, measurement_only = TRUE)

}





#' Convert a seminr measurement model to a Graphviz representation
#'
#' With the help of the \code{DiagrammeR} package this code can then be plotted in
#' various contexts.
#'
#'
# @rdname dot_graph
#' @param model Model created with \code{seminr}.
#' @param title An optional title for the plot
#' @param theme Theme created with \code{\link{seminr_theme_create}}.
#' @param ... Unused
#'
# @return The path model as a formatted string in dot language.
#' @export
#'
#' @examples
#' # - - - - - - - - - - - - - - - -
#' # Example for plotting a measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' res <- dot_graph(mobi_mm, title = "Preview measurement model")
#'
#' \dontrun{
#' DiagrammeR::grViz(res)
#' }
dot_graph.structural_model <-
  function(model,
           title = "",
           theme = NULL, ...
  ){


  unusedParams <- list(...)
  if (length(unusedParams))
    stop('Unused parameters: ', paste(unusedParams, collapse = ', '))


    if (is.null(theme)) {
      thm <- seminr_theme_get()
    } else {
      thm <- theme
    }

    # THIS IS AN ARTIFICAL MODEL THAT LETS ME REUSE THE OLD PLOTTING FUNCTION,
    # THIS is unnecessary complex(?).
    sm_constructs <- c(model[,1], model[,2]) %>% unique()
    mm_list <- list()
    for (i in sm_constructs) {
      mm_list[[i]] <- reflective(i, paste0(i,"_dummy"))
    }
    measurement_model <- do.call(constructs, mm_list)
    mm <- measurement_model %>% mm2matrix()
    mm %>% as.data.frame() -> mmodel
    a_model <- list(measurement_model = measurement_model,
                  mmMatrix = matrix(),
                  smMatrix = model,
                  outer_weights = matrix(c(1), # add only 1s
                                         ncol = length(mmodel$construct %>% unique()),
                                         dimnames = list(mmodel$measurement %>% unique, mmodel$construct %>% unique()),
                                         nrow = length(mmodel$measurement %>% unique())
                  ),
                  path_coef = matrix(c(1),
                                     ncol = length(sm_constructs),
                                     nrow = length(sm_constructs),
                                     dimnames = list(sm_constructs, sm_constructs)),
                  constructs = mmodel$construct %>% unique(),
                  mmVariables = mmodel$measurement %>% unique()
    )

    class(a_model) <- "pls_model"


    thm$sm.edge.width_multiplier <- 1
    thm$sm.edge.label.show <- FALSE

    dot_graph(a_model, title = title, theme = thm, structure_only = TRUE)

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
#' @rdname dot_graph
#' @param model Model created with \code{seminr}.
#' @param title An optional title for the plot
#' @param theme Theme created with \code{\link{seminr_theme_create}}.
#' @param measurement_only Plot only measurement part
#' @param structure_only Plot only structure part
#'
# @return The path model as a formatted string in dot language.
#' @export
#'
#' @examples
#' # - - - - - - - - - - - - - - - -
#' # Example for plotting a PLS-Model
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
#' res <- dot_graph(mobi_pls, title = "PLS-Model plot")
#'
#' \dontrun{
#' DiagrammeR::grViz(res)
#' }
dot_graph.pls_model <- function(model,
                                title = "",
                                theme = NULL,
                                measurement_only = FALSE,
                                structure_only = FALSE, ...
) {

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


  # generate components ----
  sm <- ""
  mm <- ""
  # replace needed parts do not break if-else blocks as some artificial models only work with either function
  if (measurement_only) {
    sm <- dot_component_sm_parts(model = model, theme = thm)
  } else {
    sm <- dot_component_sm(model = model, theme = thm)
  }
  if (structure_only) {
    mm <- ""
  } else {
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

dot_component_sm_parts <- function(model, theme){
  #used for plotting measurement models
  sm_nodes <- extract_sm_nodes(model, theme)
  sm_node_style <- get_sm_node_style(theme)
  glue_dot(paste0("// --------------------\n",
                  "// The structural model\n",
                  "// --------------------\n",
                  "subgraph sm {\n",
                  "rankdir = LR;\n",
                  "node [\n",
                  "<<sm_node_style>>\n",
                  "]\n",
                  "<<sm_nodes>>\n",
                  "}\n"))
}

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
    formatted_node <- paste0("'", construct, "'",
                             " [label='", construct, "\nr",squared_symbol,"=", round(model$rSquared[1, construct], theme$plot.rounding), "']")
  } else {
    formatted_node <- paste0("'", construct, "'" , "[label='",gsub("_x_","\\*", construct),"']")
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


get_value_dependent_edge_style <- function(value, theme){
  edge_style <- paste0(", style = solid")
  if (value < 0) {
    edge_style <- paste0(", style = dashed") # TODO possible color to red?
  }
  edge_style
}

# extract structural model edges from a seminr model
extract_sm_edges <- function(model, theme, weights = 1) {

  nr <- nrow(model$smMatrix)
  nc <- ncol(model$smMatrix)
  sm <- model$smMatrix
  sm_edges <- c()

  # Unicode for small mathematical symbols
  beta <- "\U0001D6FD"
  #print(beta)
  gamma <- "\U0001D6FE"
  #print(gamma)

  # for every path add an edge
  for (i in 1:nrow(sm)) {
    letter <- beta
    if ( !(sm[i,1] %in% colnames(model$rSquared))) {
      letter <- gamma # when it is determined only by exogenous variables use gamma
    }

    coef <- round(model$path_coef[sm[i, 1], sm[i,2]], theme$plot.rounding)

    edge_label <- ""
    if (theme$sm.edge.label.show) {
      edge_label <- paste0(", label = '", letter, " = ", coef, "'")
    }

    edge_weight <- paste0("weight = ", weights)
    edge_width <- paste0(", penwidth = ", abs(coef * theme$sm.edge.width_multiplier))
    edge_style <- get_value_dependent_edge_style(coef, theme)
    sm_edges <- c(sm_edges,
                  paste0("'", sm[i, 1], "' -> {'", sm[i, 2], "'}","[", edge_weight, edge_label, edge_width, edge_style, "]"))
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


# TODO: document purpose of this function
#
extract_mm_coding <- function(model) {
  construct_names <- c()
  construct_types <- c()
  for (i in seq_along(model$measurement_model)) {
    c(construct_types, names(model$measurement_model)[i]) -> construct_types
    if (names(model$measurement_model)[i] != "scaled_interaction") {
      # cannot call this as it is a function
      c(construct_names, model$measurement_model[[i]][[1]]) -> construct_names

      #c(construct_names, model$constructs[i]) -> construct_names
    } else {
      c(construct_names, model$constructs[i]) -> construct_names # can we always use this? NO order is not the same
    }
  }
  mm_coding <- matrix(nrow = length(construct_names),
                      ncol = 2,
                      data = c(construct_names, construct_types))
  colnames(mm_coding) <- c("name", "type")
  return(mm_coding)
}



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
  is_interaction <- mm_coding[index, 2] == "scaled_interaction"
  if (is_interaction) {
    return("")
  }

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

      edge_label <- ""
      if (theme$mm.edge.label.show) {
        edge_label <- paste0(", label = '", letter, " = ", loading, "'")
      }
      edge_style <- get_value_dependent_edge_style(loading, theme)
      edges <- paste0(edges,
                      "'",mm_matrix_subset[2], "' -> {'", mm_matrix_subset[1], "'}",
                      "[weight = ", weights, edge_label ,", penwidth = ", abs(loading * theme$mm.edge.width_multiplier), edge_style, "]\n")
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

        edge_label <- ""
        if (theme$mm.edge.label.show) {
          edge_label <- paste0(", label = '", letter, " = ", loading, "'")
        }
        edge_style <- get_value_dependent_edge_style(loading, theme)
        edges <- paste0(edges,
                        "'",mm_matrix_subset[i, 2], "' -> {'", mm_matrix_subset[i, 1], "'}",
                        "[weight = ", weights, edge_label,", penwidth = ", abs(loading * theme$mm.edge.width_multiplier), edge_style, "]\n")
      }
    }
  }

  # we don't show interaction term measurement items
  #edges <- gsub("\\*", "_x_", edges)
  return(edges)
}



# EXPERIMENTAL STUFF ----

hyperedge <- function(){
  dot <- "digraph {
  A [label = IV]
  B [label = DV]
  C [label = Moderator]
  empty [label = '', shape = point, width = 0, height = 0]

  A -> empty  [arrowhead = none, weight = 1000]
  empty -> B [weight = 1000]
  C -> empty [constraint = FALSE]
  C -> B
}
"
  #DiagrammeR::grViz(dot)
}

