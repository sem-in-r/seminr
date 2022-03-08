# TODO: this hijacks parts of the theme that are designed for the normal model plots
# possibly refactor to have its own settings, because being over threshold
# has a negative connotation. So for colored themes, this might be weird.


#' Creates a dot string with a network graph of constructs based on HTMT measures
#'
#' Using a bootstrapped model this functions shows which constructs show
#' insufficient discriminant validity.
#'
#' @param model A bootsrapped PLS-Model
#' @param title Optional title over the plot.
#' @param theme Optional theme to use for plotting
#' @param htmt_threshold The threshold to use under which constructs are highlighted (default = 1.0)
#' @param omit_threshold_edges Whether or not to omit constructs that have low HTMT values (default = TRUE)
#' @param use_ci Whether or not to rely on the upper threshold of the CI instead of the bootstrapped mean (default = FALSE)
#'
#' @return Returs a dot string of the plot
#' @export
#'
# @examples
dot_graph_htmt <- function(model, title = "HTMT Plot", theme = seminr::seminr_theme_get(), htmt_threshold = 1,
                           omit_threshold_edges = TRUE, use_ci = FALSE) {
  if (!("boot_seminr_model" %in% class(model))) {
    stop("Plotting HTMT models only works with bootstrapped models")
  }

  thm <- theme
  thm$plot.title <- title

  global_style <- get_global_htmt_style(thm)

  thm$sm.node.width  <- get_construct_element_size(model, thm)[1]
  thm$sm.node.height <- get_construct_element_size(model, thm)[2]

  htmt_component <- dot_component_htmt(model, thm, htmt_threshold, omit_threshold_edges, use_ci)


  res <- glue_dot(paste0("graph G {\n",
                  "\n<<global_style>>\n",
                  "\n<<htmt_component>>",
                  "\n}\n"))

  res
}


#' Plots a network graph of constructs based on HTMT measures
#'
#' Using a bootstrapped model this functions shows which constructs show
#' insufficient discriminant validity.
#'
#' @param model A bootsrapped PLS-Model
#' @param title Optional title over the plot.
#' @param theme Optional theme to use for plotting
#' @param htmt_threshold The threshold to use under which constructs are highlighted (default = 1.0)
#' @param omit_threshold_edges Whether or not to omit constructs that have low HTMT values (default = TRUE)
#' @param use_ci Whether or not to rely on the upper threshold of the CI instead of the bootstrapped mean (default = FALSE)
#'
#' @return Returs a dot string of the plot
#' @export
#'
# @examples
plot_htmt <- function(model, title = "HTMT Plot", theme = seminr::seminr_theme_get(), htmt_threshold = 1,
                      omit_threshold_edges = TRUE, use_ci = FALSE) {
  if (!("boot_seminr_model" %in% class(model))) {
    stop("Plotting HTMT models only works with bootstrapped models")
  }

  pl <- dot_graph_htmt(model, title, theme, htmt_threshold, omit_threshold_edges, use_ci)

  res <- DiagrammeR::grViz(pl)
  set_last_seminr_plot(res)
  return(res)
}




# internal functions ----

#' Get dot string for global theme options
#' @keywords internal
#' @param theme a theme
#' @param layout The layout engine (default: dot)
get_global_htmt_style <- function(theme, layout = "dot") {
  glue_dot(paste0("// ----------------------\n",
                  "// General graph settings\n",
                  "// ----------------------\n",
                  "graph [\n",
                  "charset = \"UTF-8\",\n",
                  "layout = ", layout, ",\n",
                  "label = \"<<theme$plot.title>>\",\n",
                  "fontsize = <<theme$plot.title.fontsize>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "rankdir = LR,\n",
                  "labelloc = t,\n",
                  "ranksep = 0.5,\n",
                  "nodesep = 0.5,\n",
                  "splines = <<theme$plot.splines>>\n",
                  "]\n"))
}



extract_htmt_edges <- function(model, theme, htmt_threshold = 1, omit_threshold_edges = TRUE, use_ci = FALSE){
  htmt <- model$HTMT_descriptives
  constructs <- model$constructs


  smry <- summary(model)
  thm <- theme

  smry$bootstrapped_HTMT
  gr <- ""
  # iterate upper triangle
  for (i in 1:(length(constructs) - 1)) {
    for (j in (i + 1):(length(constructs))) {

      row_index <- constructs[i]
      col_index <- paste0(constructs[j], " Boot Mean")

      smry_index <- paste0(constructs[i], "  ->  ", constructs[j])


      value <- round(smry$bootstrapped_HTMT[smry_index,2], thm$plot.rounding)

      ciupper <- round(smry$bootstrapped_HTMT[smry_index,6], thm$plot.rounding)
      cilower <- round(smry$bootstrapped_HTMT[smry_index,5], thm$plot.rounding)

      cistring <- paste0("95% CI [", cilower, "; ", ciupper, "]")

      #
      if (use_ci) {
        cmp_value <- ciupper
      } else {
        cmp_value <- value
      }

      edge_label <- paste0(value, "<BR/>", cistring)

      penwidth <- (value * thm$mm.edge.width_multiplier) + 0.2

      edgecolor <- if (cmp_value > htmt_threshold) {
        "red"
        #TODO
        #thm$sm.edge.positive.color
      } else {
        "black"
        #thm$sm.edge.negative.color
      }
      edgestyle <- if (cmp_value > htmt_threshold) {
        thm$sm.edge.positive.style
      } else {
        thm$sm.edge.negative.style
      }

      # if omit
      if (!omit_threshold_edges || cmp_value > htmt_threshold) {
        gr <- paste0(gr, "\"", constructs[i], "\" -- \"", constructs[j], "\"",
                     " [label = <", edge_label, ">",
                     ", penwidth = ", penwidth,
                     ", weight = ", round(length(constructs)/value, thm$plot.rounding),
                     ", color = ", edgecolor,
                     ", style = ", edgestyle,
                     "]",
                     "\n")
      }
    }
  }

  gr
}


get_htmt_edge_style <- function(theme){
  minlen_str <- ""
  if (!is.na(theme$sm.edge.minlen)) {
    minlen_str <- glue_dot("minlen = <<theme$sm.edge.minlen>>,\n")
  }
  glue_dot(paste0("color = <<theme$sm.edge.positive.color>>,\n", # fallback
                  "fontsize = <<theme$sm.edge.label.fontsize>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "<<minlen_str>>",
                  #"constraint=false,", # TODO: consider optional parameter
                  "dir = both,\n",
                  "arrowhead = none,\n",
                  "arrowtail = none"))

}

# format structural model node where appropriate
format_htmt_node <- function(construct, model, theme){

  # Init node as empty string
  formatted_node <- ""

  # detect construct type
  shape_string <- get_sm_node_shape(model, construct, theme)
  label_string <- format_exo_node_label(theme, construct)
  formatted_node <- paste0("\"", construct, "\"" , " [label=<",
                             label_string,
                             ">", shape_string, "]")

  return(formatted_node)
}

#' Helper function that applies formatting to each construct
#'
#' @param model the model to use
#' @param theme the theme to use
#'
#' @return Returns a string of the structural model in dot notation.
extract_htmt_nodes <- function(model, theme) {
  htmt_nodes <- model$constructs

  htmt_nodes <- sapply(htmt_nodes, format_htmt_node, model, theme)
  htmt_nodes <- paste0(htmt_nodes, collapse = "\n")
  return(htmt_nodes)
}

get_htmt_node_style <- function(theme) {
  glue_dot(paste0("shape = ellipse,\n",         #fall-back if something breaks
                  "color = <<theme$sm.node.color>>,\n",
                  "fillcolor = <<theme$sm.node.fill>>,\n",
                  "style = filled,\n",
                  "fontsize = <<theme$sm.node.label.fontsize>>,\n",
                  "height = <<theme$sm.node.height>>,\n",
                  "width = <<theme$sm.node.width>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "fixedsize = true\n"))
}


# construct structural model subgraph
dot_component_htmt <- function(model, theme, htmt_threshold, omit_threshold_edges, use_ci) {
  htmt_nodes <- extract_htmt_nodes(model, theme)
  htmt_node_style <- get_htmt_node_style(theme)
  htmt_edges <- extract_htmt_edges(model, theme, htmt_threshold, omit_threshold_edges, use_ci)
  htmt_edge_style <- get_htmt_edge_style(theme)
  glue_dot(paste0("\n// --------------------\n",
                  "// The htmt model\n",
                  "// --------------------\n",
                  "node [\n",
                  "<<htmt_node_style>>\n",
                  "]\n",
                  #"<<htmt_nodes>>\n",     # this auto removes uneccesary nodes
                  "edge [\n",
                  "<<htmt_edge_style>>\n",
                  "]\n",
                  "<<htmt_edges>>\n",
                  "\n"))
}


