# To allow some dot notation here and there
globalVariables(c("."))

# Naming Conventions ----
# extract_xxxx extracts information from the model
# get_xxxx retrieves information from the theme (styles, etc.)
# format_xxx applies more/less smart formatting to content
# dot_xxxx generates dot code



# Main exported methods ----


#' Plot various SEMinR models
#'
#' With the help of the \code{DiagrammeR} package this dot graph can then be plotted in
#' various in RMarkdown, shiny, and other contexts.
#' Depending on the type of model, different parameters can be used.
#' Please check the \code{\link{dot_graph}} function for additional parameters.
#'
#' @param x The model description
#' @param title An optional title for the plot
#' @param theme Theme created with \code{\link{seminr_theme_create}}.
#' @param ... Please check the \code{\link{dot_graph}} for the additional parameters
#'
#' @return Returns the plot.
#' @export
plot.seminr_model <- function(x,
                       title = "",
                       theme = NULL,
                       ...) {

  query_install("DiagrammeR", "Alternatively use the dot_graph() function to create a dot graph.")

  model <- x
    # lavaan models
    if (inherits(model, "cfa_model")) {
      message("Plotting of lavaan models using semPlot.")
      dot_graph.cfa_model(model, ...)
      return()
    }
    if (inherits(model, "cbsem_model")) {
      message("Plotting of lavaan models using semPlot.")
      dot_graph.cbsem_model(model, ...)
      return()
    }

    if (inherits(title, "seminr_theme")) {
      warning(paste0("You have supplied a theme in the title parameter. ",
                     "Please use named parameters to use a specific theme: ",
                     "plot(model, theme = thm).")
      )
    }

    # actual plotting
    if(requireNamespace("DiagrammeR", quietly = TRUE)){
      res <- DiagrammeR::grViz(dot_graph(model, title, theme, ...))
      set_last_seminr_plot(res)
      return(res)
    } else {
      return(NULL)
    }

}


query_install <- function(pkg_name = "DiagrammeR", failure_msg=""){
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    if (interactive()) {
      x <- readline(paste0(
        "----------------------------------------------------------------------\n",
        "This function requires the ", pkg_name, " package.\n",
        "You can install it by calling: install.packages(\"", pkg_name, "\")\n",
        "Do you want to install ", pkg_name, " right now (Y/n)? "
      ))
      if (x == "Y") {
        utils::install.packages(pkg_name)
      } else {
        stop(
          paste0("You have selected no. Please use a capital Y to agree with installing ",pkg_name, " on your machine.\n",
                 failure_msg
          )
        )
      }
    }
  }
}




#' Saves a SEMinR model plot to file
#'
#' Saves a SEMinR model plot to a graphical file. Default output is RPlots.pdf.
#'
#' @param filename The name of the file output (can be png, pdf, webp, ps, or svg.)
#' @param plot A plot that is created from the \code{\link{plot}} function. By default it uses the last plot created.
#' @param width An optional parameter for width in pixels.
#' @param height An optional parameter for height in pixels.
#'
#' @return Does not return a value
#' @export
#'
#' @examples
#' mobi <- mobi
#'
#' # seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' # seminr syntax for creating structural model
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' # estimate the model
#' mobi_pls <- estimate_pls(data = mobi,
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm)
#' \dontrun{
#' # generate the plot
#' plot(mobi_pls)
#' # save to file
#' save_plot("myplot.pdf")
#' }
#'
save_plot <- function(filename = "RPlot.pdf", plot = last_seminr_plot(), width = NULL, height = NULL){
  query_install("DiagrammeRsvg")
  query_install("rsvg")

  # prevent failure quietly
  if(!requireNamespace("DiagrammeRsvg", quietly = TRUE)){
    return(NULL)
  }
  if(!requireNamespace("rsvg", quietly = TRUE)){
    return(NULL)
  }


  if (is.null(plot)) {
    stop("No compatible plot was created.")
  }

  # generate svg string

  svg <- charToRaw( DiagrammeRsvg::export_svg(plot) )

  file_extension <- tolower(tools::file_ext(filename))
  result = switch(
    file_extension,
    "pdf" = {rsvg::rsvg_pdf(svg, filename, width = width, height = height)},
    "png" = {rsvg::rsvg_png(svg, filename, width = width, height = height)},
    "ps" = {rsvg::rsvg_ps(svg, filename, width = width, height = height)},
    "svg" = {rsvg::rsvg_svg(svg, filename, width = width, height = height)},
    "webp" = {
      query_install("webp")
      rsvg::rsvg_webp(svg, filename, width = width, height = height)
      },
    "raw" = {rsvg::rsvg_raw(svg, filename, width = width, height = height)},

    # else
    {message(paste0("Unsuported file type: '", file_extension, "'. Please use either png, pdf, ps, webp, or svg"))}
  )


}


# ___________________  ----
# DOT GRAPH (main function)----

#' Generate a dot graph from various SEMinR models
#'
#' With the help of the \code{DiagrammeR} package this dot graph can then be plotted in
#' various in RMarkdown, shiny, and other contexts.
#' Depending on the type of model, different parameters can be used.
#'
#' Current limitations:
#' - Only plots PLS Models
#' - no higher order constructs
#'
#' @param model The model description
#' @param title An optional title for the plot
#' @param theme Theme created with \code{\link{seminr_theme_create}}.
#' @param ... Additional parameters
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
#' # adapt nboot for better results
#' mobi_boot <- bootstrap_model(mobi_pls, nboot = 20, cores = 1)
#' # generate dot-Notation
#' res <- dot_graph(mobi_pls, title = "PLS-Model plot")
#'
#' \dontrun{
#' DiagrammeR::grViz(res)}
#'
#' # generate dot-Notation
#' res <- dot_graph(mobi_boot, title = "Bootstrapped PLS-Model plot")
#'
#' \dontrun{
#' DiagrammeR::grViz(res)}
#'
dot_graph <- function(model,
                      title = "",
                      theme = NULL,
                      ...) {
  UseMethod("dot_graph")
}

#' Plotting of confirmatory factor analysis models using semPLOT
#'
#' For a full description of parameters for lavaan models see semPaths method in the semPlot package.
#'
#' @rdname dot_graph
#' @param model the CFA model
#' @param title Unused
#' @param theme Unused
#' @param what The metric to use for edges ("path", "est", "std", "eq", "col")
#' @param whatLabels The metric to use for edge labels
#' @param ... Parameters passed to the \link[semPlot]{semPaths} function
#' @export
dot_graph.cfa_model <- function(model, title = "", theme = NULL, what = "std", whatLabels = "std", ...){
  query_install("semPlot", "Plotting models from lavaan is not implemented yet. semPlot is required as a fallback.")
  if(requireNamespace("semPlot", quietly = TRUE)){
    return(semPlot::semPaths(model$lavaan_output, what = what, whatLabels = whatLabels,...))
  } else {
    return("")
  }
}

#' Plotting of covariance based SEMs models using semPLOT
#'
#' @rdname dot_graph
#' @param model the CBSEM model
#' @param title Unused
#' @param theme Unused
#' @param what The metric to use for edges ("path", "est", "std", "eq", "col")
#' @param whatLabels The metric to use for edge labels
#' @param ... Parameters passed to the \link[semPlot]{semPaths} function
#' @export
dot_graph.cbsem_model <- function(model, title = "", theme = NULL, what = "std", whatLabels = "std", ...){
  query_install("semPlot", "Plotting models from lavaan is not implemented yet. semPlot is required as a fallback.")
  if(requireNamespace("semPlot", quietly = TRUE)){
    return(
      semPlot::semPaths(model$lavaan_output, what = what, whatLabels = whatLabels,...)
    )
  } else {
    return("")
  }
}

#' @export
dot_graph.default <- function(...){
  stop("Whoops. This shouldn't have happened. Did you use a SEMinR model? If yes, please let us know if this happens and how.")
}

#' Convert a seminr measurement model to a Graphviz representation
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
#' dot_graph(mobi_mm, title = "Preview measurement model")
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
  mm <- mm2matrix(model)
  as.data.frame(mm) -> mmodel

  hocs <- model$higher_order_composite

  a_model <- list(measurement_model = model,
                mmMatrix = mm,
                smMatrix = matrix(rep(unique(mmodel$construct),2),
                                  ncol = 2,
                                  nrow = length(unique(mmodel$construct))),
                outer_weights = matrix(c(1), # add only 1s
                                       ncol = length(unique(mmodel$construct) ),
                                       dimnames = list(unique(mmodel$measurement),
                                                       unique(mmodel$construct) ),
                                       nrow = length(unique(mmodel$measurement) )
                                       ),
                outer_loadings = matrix(c(1), # add only 1s
                                       ncol = length(unique(mmodel$construct) ),
                                       dimnames = list(unique(mmodel$measurement),
                                                       unique(mmodel$construct) ),
                                       nrow = length(unique(mmodel$measurement) )
                ),
                constructs = unique(mmodel$construct),
                mmVariables = unique(mmodel$measurement)
  )
  if (length(hocs) > 0) {
    a_model$hoc <- TRUE
  }
  class(a_model) <- "pls_model"


  # adjust themes to correct for artifical information
  thm$mm.edge.width_multiplier <- 1
  thm$mm.edge.label.show <- FALSE
  dot_graph(a_model, title = title, theme = thm, measurement_only = TRUE)
}





#' Convert a seminr structural model to a Graphviz representation
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
#' # Example for plotting a structural model
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#' res <- dot_graph(mobi_sm, title = "Preview structural model")
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
    sm_constructs <- unique( c(model[,1], model[,2]) )
    mm_list <- list()
    for (i in sm_constructs) {
      mm_list[[i]] <- reflective(i, paste0(i,"_dummy"))
    }
    measurement_model <- do.call(constructs, mm_list)
    mm <- mm2matrix( measurement_model )
    as.data.frame(mm) -> mmodel
    a_model <- list(measurement_model = measurement_model,
                  mmMatrix = matrix(),
                  smMatrix = model,
                  outer_weights = matrix(c(1), # add only 1s
                                         ncol = length(unique(mmodel$construct)),
                                         dimnames = list(unique(mmodel$measurement),
                                                         unique(mmodel$construct)),
                                         nrow = length(unique(mmodel$measurement))
                  ),
                  path_coef = matrix(c(1),
                                     ncol = length(sm_constructs),
                                     nrow = length(sm_constructs),
                                     dimnames = list(sm_constructs, sm_constructs)),
                  constructs = unique(mmodel$construct),
                  mmVariables = unique(mmodel$measurement)
    )

    class(a_model) <- "pls_model"


    thm$sm.edge.width_multiplier <- 1
    thm$sm.edge.label.show <- FALSE

    dot_graph(a_model, title = title, theme = thm, structure_only = TRUE)
}



#' Convert a seminr model to Graphviz representation
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
# @examples
dot_graph.specified_model <-  function(model,
                                        title = "",
                                        theme = NULL,
                                        measurement_only = FALSE,
                                        structure_only = FALSE, ...
) {
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



   #<- unique(model$structural_model[,1])
  #mm_list <- list()
  #for (i in sm_constructs) {
  #  mm_list[[i]] <- reflective(i, paste0(i,"_dummy"))
  #}
  #measurement_model <- do.call(constructs, mm_list)
  #mm <- mm2matrix( measurement_model )
  #as.data.frame(mm) -> mmodel

  measurement_model <- model$measurement_model
  mm <- mm2matrix( measurement_model )
  as.data.frame(mm) -> mmodel
  sm_constructs <- unique(mmodel$construct)


  weight_matrix <- matrix(c(1), # add only 1s
         ncol = length(unique(mmodel$construct)),
         dimnames = list(unique(mmodel$measurement),
                         unique(mmodel$construct)),
         nrow = length(unique(mmodel$measurement))
  )

  path_matrix <- matrix(c(1),
                        ncol = length(sm_constructs),
                        nrow = length(sm_constructs),
                        dimnames = list(sm_constructs, sm_constructs))



  # specify artificial model
  a_model <- list(measurement_model = measurement_model,
                  mmMatrix = mm,
                  smMatrix = model$structural_model,
                  outer_weights = weight_matrix,
                  outer_loadings = weight_matrix,
                  path_coef = path_matrix,
                  constructs = unique(mmodel$construct),
                  mmVariables = unique(mmodel$measurement)
  )

  class(a_model) <- "pls_model"


  thm$sm.edge.width_multiplier <- 1
  thm$sm.edge.label.show <- FALSE
  thm$mm.edge.width_multiplier <- 1
  thm$mm.edge.label.show <- FALSE

  dot_graph(a_model, title = title, theme = thm, measurement_only = measurement_only, structure_only, structure_only)
}


#' Convert a seminr model to Graphviz representation
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
# @examples
dot_graph.boot_seminr_model <- function(model,
                                title = "",
                                theme = NULL,
                                measurement_only = FALSE,
                                structure_only = FALSE, ...
) {
  # the origingal pls method is capable of plotting boot strapped models
  dot_graph.pls_model(model, title, theme, measurement_only, structure_only, ...)
}


#' Convert a seminr model to Graphviz representation
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
dot_graph.pls_model <- function(model,
                                title = "",
                                theme = NULL,
                                measurement_only = FALSE,
                                structure_only = FALSE, ...
) {

  if (is.null(theme)) {
    thm <- seminr_theme_get()
  } else {
    if (inherits(theme, "function")) {
      thm <- theme()
    } else {
      thm <- theme
    }
  }

  if (thm$plot.title == "") {
    thm$plot.title <- title
  }

  global_style <- get_global_style(theme = thm)

  # rewrite size defaults in theme
  thm$sm.node.width  <- get_construct_element_size(model, thm)[1]
  thm$sm.node.height <- get_construct_element_size(model, thm)[2] * 2 # two lines, could be optimized

  thm$mm.node.width <- get_manifest_element_size(model, thm)[1]
  thm$mm.node.height <- get_manifest_element_size(model, thm)[2]


  # generate components ---
  sm <- ""
  mm <- ""
  # replace needed parts
  # do not change the order in if-else statement as some artificial models only work with either function
  if (measurement_only) {
    sm <- dot_component_sm_parts(model = model, theme = thm)
  } else {
    sm <- dot_component_sm(model = model, theme = thm, structure_only = structure_only)
  }
  if (structure_only) {
    mm <- ""
  } else {
    mm <- dot_component_mm(model = model, theme = thm)
  }
  # do not change end - - - -

  glue_dot(paste0("digraph G {\n",
                  "\n<<global_style>>\n",
                  "\n<<sm>>",
                  "\n<<mm>>",
                  "\n}\n"))
}







# ___________________  ----
# Graph options ------------------

#' Get dot string for global theme options
#' @keywords internal
#' @param theme a theme
#' @param layout The layout engine (default: dot)
get_global_style <- function(theme, layout = "dot") {
  glue_dot(paste0("// ----------------------\n",
                  "// General graph settings\n",
                  "// ----------------------\n",
                  "graph [\n",
                  "charset = \"UTF-8\",\n",
                  "layout = ", layout, ",\n",
                  "label = \"<<theme$plot.title>>\",\n",
                  "fontsize = <<theme$plot.title.fontsize>>,\n",
                  "fontcolor = <<theme$plot.title.fontcolor>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "rankdir = LR,\n",
                  "labelloc = t,\n",
                  "splines = <<theme$plot.splines>>\n",
                  "bgcolor = <<theme$plot.bgcolor>>\n",
                  "]\n"))
}

# 1. Node options ----

#' Gets the optimal size for construct elements in the plot
#'
#' Currently orients on reflective theme settings
#'
#' @param model the model to use
#' @param theme the theme to use
#'
#' @return Returns a two-element vector with c(width, height)
get_construct_element_size <- function(model, theme) {

  r_offset <- get_sm_element_offset(theme$construct.reflective.shape)
  a_offset <- get_sm_element_offset(theme$construct.compositeA.shape)
  b_offset <- get_sm_element_offset(theme$construct.compositeB.shape)

  c_width_offset   <- max(r_offset$width, a_offset$width, b_offset$width)
  c_height_offset <- max(r_offset$height, a_offset$height, b_offset$height)

  construct_width <- max(
    graphics::strwidth(model$constructs, font = theme$sm.node.label.fontsize, units = "in")
  ) + c_width_offset
  construct_height <- max(
    graphics::strheight(model$constructs, font = theme$sm.node.label.fontsize, units = "in")
  ) + c_height_offset

  c(construct_width, construct_height)
}

#' Gets the optimal size for manifest elements in the plot
#'
#' Currently orients on reflective theme settings
#'
#' @param model the model to use
#' @param theme the theme to use
#'
#' @return Returns a two-element vector with c(width, height)
get_manifest_element_size <- function(model, theme) {
  r_offset <- get_mm_element_offset(theme$manifest.reflective.shape)
  a_offset <- get_mm_element_offset(theme$manifest.compositeA.shape)
  b_offset <- get_mm_element_offset(theme$manifest.compositeB.shape)

  i_width_offset   <- max(r_offset$width, a_offset$width, b_offset$width)
  i_height_offset <- max(r_offset$height, a_offset$height, b_offset$height)

  item_width <- max(
    graphics::strwidth(model$mmVariables,font = theme$mm.node.label.fontsize, units = "in")
  ) + i_width_offset
  item_height <- max(
    graphics::strheight(model$mmVariables,font = theme$mm.node.label.fontsize, units = "in")
  ) + i_height_offset

  c(item_width, item_height)
}



# 2. Edge options ----

# generic formatting function for bootstrapped edges
format_edge_boot_label <- function(template, variable, value, tvalue, pvalue, stars, civalue) {
  content <- glue::glue(template)

  paste0(", label = < ",
         content,
         " >")
}

# generic formatting function for edges
format_edge_label <- function(template, variable, value) {
  glue::glue(template)
}

#' Returns the type of a construct from a model
#' @param model the model to get the type from
#' @param construct the character string name of the construct
#' @return Returns a character string
get_construct_type <- function(model, construct) {
  #if (!(construct %in% model$constructs)) {
  #  stop(paste("Construct", construct, "does not exist")) # scaled interactions ?
  #}
  if (grepl("\\*", construct)) {
    return("interaction")
  }
  for (i in 1:length(model$measurement_model)) {
    cst <- model$measurement_model[[i]]
    # warning interaction are functions do not access their indexes
    if (!inherits(cst, "function")) {
      if (cst[[1]] == construct) {
        construct_type <- cst[[3]]
      }
    }
  }

  return(construct_type)
}

#' extract bootstrapped statistics from an edge using a row_index
#'
#' @param ltbl a table of bootstrapped values (weights, loadings, path coefficients)
#' @param row_index the index for the specific edge to extract
#' @param model the model to use
#' @param theme the theme to use
extract_bootstrapped_values <- function(ltbl, row_index, model, theme) {

  t_value <- ltbl[rownames(ltbl) == row_index, 4]

  pvalue <- stats::pt(abs(t_value), nrow(model$data) - 1, lower.tail = FALSE)

  list(
    mean = round(ltbl[rownames(ltbl) == row_index, 1], theme$plot.rounding),
    lower = round(ltbl[rownames(ltbl) == row_index, 5], theme$plot.rounding),
    upper = round(ltbl[rownames(ltbl) == row_index, 6], theme$plot.rounding),
    tvalue = round(t_value, theme$plot.rounding),
    p = pvalue
  )
}


# Needs tweaking ----

# gets the offsets for predefined shapes
get_mm_element_offset <- function(element) {
  offset_table <- data.frame(
    shape  = c("box", "rectangle", "ellipse", "hexagon"),
    width  = c(0.0,      0,          0.4,       0.4),
    height = c(0.05,     0.05,       0.4,       0.3)
  )

  offset_table[offset_table$shape == element, 2:3]
}

# gets the offsets for predefined shapes
get_sm_element_offset <- function(element) {
  offset_table <- data.frame(
    shape  = c("box", "rectangle", "ellipse", "hexagon"),
    width  = c(0.2,      0.2,          0.4,       0.4),
    height = c(0.1,      0.1,         0.4,       0.3)
  )

  offset_table[offset_table$shape == element, 2:3]
}




# ___________________  ----
# 1. Structural Model ----------------------

# construct structural model using subgraphs
dot_component_sm <- function(model, theme, structure_only = FALSE) {
  sm_nodes <- extract_sm_nodes(model, theme, structure_only = structure_only)
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


# Special case used when plotting  measurement models (ignores structural model edges!)
dot_component_sm_parts <- function(model, theme){
  #used for plotting measurement models
  # This is a "hacky" solution. Because we create artificial models
  # and did not want to create an artificial measurement model
  # this function is used to plot only the SM part.
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




# 1.1 SM-Nodes ----

#' Helper function that applies formatting to each construct
#'
#' @param model the model to use
#' @param theme the theme to use
#' @param structure_only is this called in a structure_only model
#'
#' @return Returns a string of the structural model in dot notation.
extract_sm_nodes <- function(model, theme, structure_only = FALSE) {
  sm_nodes <- model$constructs


  # Add additional SM nodes for submodel
  for (construct in model$constructs) {
    construct_type <- get_construct_type(model, construct)

    if (startsWith(construct_type, "HOC") && !structure_only) {

      # row_index <- grepl(construct, model$mmMatrix[,1])
      row_index <- model$mmMatrix[,1] == construct
      result <- model$mmMatrix[row_index, 2]
      sm_nodes <- c(sm_nodes, result)
    }
  }
  sm_nodes <- sapply(sm_nodes, format_sm_node, model, theme)
  sm_nodes <- paste0(sm_nodes, collapse = "\n")
  return(sm_nodes)
}










# format structural model node where appropriate
format_sm_node <- function(construct, model, theme){

  # this is the unicode symbol for ^2
  if ( theme$plot.specialcharacters ) {
    squared_symbol <- "\U00B2"
  } else {
    squared_symbol <- "^2"
  }

  # Init node as empty string
  formatted_node <- ""

  # decide whether or not to use adj r^2
  if (theme$plot.adj) {
    # the index in the rSquared table for adj. r^2 is 2
    r_index <- 2
    r_string <- "adj. "
  } else {
    # the index in the rSquared table for r^2 is 1
    r_index <- 1
    r_string <- ""
  }

  # detect construct type
  shape_string <- get_sm_node_shape(model, construct, theme)

  #detect if exogenous construct
  if (construct %in% colnames(model$rSquared)) {
    rstring <- paste0(r_string, "r", squared_symbol, " = ",
                      round(model$rSquared[r_index, construct], theme$plot.rounding))

    label_string <- format_endo_node_label(theme, construct, rstring)
    formatted_node <- paste0("\"", construct, "\" ",
                             "[label=<", label_string,
                             ">", shape_string, "]")
  } else {
    label_string <- format_exo_node_label(theme, construct)
    formatted_node <- paste0("\"", construct, "\"" , " [label=<",
                             label_string,
                             ">", shape_string, "]")
  }
  return(formatted_node)
}


# returns the style of all SM nodes
get_sm_node_style <- function(theme) {
  glue_dot(paste0("shape = ellipse,\n",         #fall-back if something breaks
                  "color = <<theme$sm.node.color>>,\n",
                  "fillcolor = <<theme$sm.node.fill>>,\n",
                  "style = filled,\n",
                  "fontsize = <<theme$sm.node.label.fontsize>>,\n",
                  "fontcolor = <<theme$sm.node.label.fontcolor>>,\n",
                  "height = <<theme$sm.node.height>>,\n",
                  "width = <<theme$sm.node.width>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "fixedsize = true\n"))
}


#' Get a string to insert into a node specification using the themed shape
#'
#' @param model the model to use
#' @param construct the construct to use
#' @param theme the theme to use
#'
#' @return Returns a string that determines the shape of a node
get_sm_node_shape <- function(model, construct, theme) {
  construct_type <- get_construct_type(model, construct)

  result <- switch(construct_type,
                   "interaction" = ", shape = ellipse",
                   "C" = paste0(", shape = ", theme$construct.reflective.shape),
                   "B" = paste0(", shape = ", theme$construct.compositeB.shape),
                   "A" = paste0(", shape = ", theme$construct.compositeA.shape),
                   "HOCA" = paste0(", shape = ", theme$construct.compositeA.shape),
                   "HOCB" = paste0(", shape = ", theme$construct.compositeB.shape),
                   "UNIT" = paste0(", shape = ", theme$construct.compositeB.shape)
  )
  return(result)
}


#' Helps to render a node label for exogenous variables
#'
#' @param theme the theme to use
#' @param name the content of the name placeholder
#'
#' @return Returns the formatted string
format_exo_node_label <- function(theme, name) {
  glue::glue(theme$sm.node.exo.template)
}

#' Helps to render a node label for endogenous variables
#'
#' @param theme the theme to use
#' @param name the content of the name placeholder
#' @param rstring the content of the rstring placeholder
#'
#' @return Returns the formatted string
format_endo_node_label <- function(theme, name, rstring) {
  glue::glue(theme$sm.node.endo.template)
}



# 1.2 SM-Edges ----

# extract structural model edges from a seminr model
extract_sm_edges <- function(model, theme, weights = 1) {

  # Get information about model
  nr <- nrow(model$smMatrix)
  nc <- ncol(model$smMatrix)
  sm <- model$smMatrix

  # start with empty set of edges
  sm_edges <- c()

  # Unicode for small mathematical symbols
  # TODO: does not work in <B> sections, yet(?)
  if ( theme$plot.specialcharacters ) {
    beta <- "\U0001D6FD"
    gamma <- "\U0001D6FE" # non-bold
    gamma <- "\U0001D738" # bold
  } else {
    beta <- "beta"
    gamma <- "gamma"
  }

  # for every path add an edge
  for (i in 1:nrow(sm)) {

    # since one estimation technique is used the default is to used betas
    if (theme$sm.edge.label.all_betas) {
        letter <- beta
    } else {
      if ( !(sm[i,1] %in% colnames(model$rSquared))) {
        letter <- gamma # when it is determined only by exogenous variables use gamma
      } else {
        letter <- beta
      }
    }

    # build label components
    if ("boot_seminr_model" %in% class(model)) {
      # format bootstrapped ---
      # create a summary for summary stats
      smry <- summary(model)
      row_index <- paste0(sm[i, 1], "  ->  ", sm[i,2])
      ltbl <- smry$bootstrapped_paths


      boot_values <- extract_bootstrapped_values(ltbl, row_index, model, theme)

      # bmean <- round(ltbl[rownames(ltbl) == row_index, 2], theme$plot.rounding)
      # blower <- round(ltbl[rownames(ltbl) == row_index, 5], theme$plot.rounding)
      # bupper <- round(ltbl[rownames(ltbl) == row_index, 6], theme$plot.rounding)
      # bt <- ltbl[rownames(ltbl) == row_index, 4]
      #
      # # TODO: Verify method to calculate p values (seems correct, maybe user land?)
      # bp <- stats::pt(abs(bt), nrow(model$data) - 1, lower.tail = FALSE)

      # show element depending on theme
      if (theme$sm.edge.boot.show_t_value) {
        tvalue <- paste0("t = ", round(boot_values[["tvalue"]], theme$plot.rounding))
      } else
        tvalue <- ""

      if (theme$sm.edge.boot.show_p_value) {
        pvalue <- paste0("p ", pvalr(boot_values[["p"]], html = TRUE))
      } else
        pvalue <- ""

      if (theme$sm.edge.boot.show_p_stars) {
        stars <- psignr(boot_values[["p"]], html = TRUE)
      } else
        stars <- ""

      if (theme$sm.edge.boot.show_ci) {
        civalue <- paste0("95% CI [", boot_values[["lower"]], ", ", boot_values[["upper"]], "]")
      } else
        civalue <- ""

      edge_width <- paste0(", penwidth = ",
                           abs(boot_values[["mean"]] * theme$sm.edge.width_multiplier) +
                             theme$sm.edge.width_offset)
      edge_style <- get_value_dependent_sm_edge_style(boot_values[["mean"]], theme)
      coef <- boot_values[["mean"]]
    } else {
      # format regular pls model ---
      tvalue <- ""
      pvalue <- ""
      civalue <- ""
      stars <- ""
      coef <- round(model$path_coef[sm[i, 1], sm[i,2]], theme$plot.rounding)
      edge_width <- paste0(", penwidth = ", (abs(coef * theme$sm.edge.width_multiplier) + theme$sm.edge.width_offset))
      edge_style <- get_value_dependent_sm_edge_style(coef, theme)
    }

    # build the label
    if (theme$sm.edge.label.show) {
      edge_label <- format_edge_boot_label(theme$sm.edge.boot.template, variable = letter, value = coef, tvalue, pvalue, stars, civalue )
    } else {
      edge_label <- ""
    }

    if (theme$plot.randomizedweights) {
      # Does this help with determinism in the layout?
      weights <- weights + stats::runif(1)
    }
    # add the weight
    edge_weight <- paste0("weight = ", weights)
    sm_edges <- c(sm_edges,
                  paste0("\"", sm[i, 1], "\" -> {\"", sm[i, 2], "\"}","[", edge_weight, edge_label, edge_width, edge_style, "]"))
  }
  sm_edges <- paste0(sm_edges, collapse = "\n")
  return(sm_edges)
}


# style for all SM edges
get_sm_edge_style <- function(theme){
  minlen_str <- ""
  if (!is.na(theme$sm.edge.minlen)) {
    minlen_str <- glue_dot("minlen = <<theme$sm.edge.minlen>>,\n")
  }
  glue_dot(paste0("color = <<theme$sm.edge.positive.color>>,\n", # fallback
                  "fontsize = <<theme$sm.edge.label.fontsize>>,\n",
                  "fontcolor = <<theme$sm.edge.label.fontcolor>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "<<minlen_str>>",
                  #"constraint=false,", # TODO: consider optional parameter
                  "dir = both,\n",
                  "arrowhead = normal,\n",
                  "arrowtail = none"))

}

#' Formats the style of the structural model edges
#'
#' @param value value to compare for negativity
#' @param theme the theme to use
#'
#' @return Returns the style for the edge (both style and color)
get_value_dependent_sm_edge_style <- function(value, theme){
  edge_style <- paste0(", style = ", theme$sm.edge.positive.style,
                       ", color = ", theme$sm.edge.positive.color)
  if (value < 0) {
    edge_style <- paste0(", style = ", theme$sm.edge.negative.style,
                         ", color = ", theme$sm.edge.negative.color)
  }
  edge_style
}



# ___________________  ----
# 2. Measurement Model ----------------------



#' Tests whether the i_th construct is endogenous or not
#'
#' @param model the model object
#' @param index the index of the construct to test
#'
#' @return whether the construct is endogenous or not
#' @export
is_sink <- function(model, index) {
  # get the mm_coding
  mm_coding <- extract_mm_coding(model)


  # Code explanation
  # as the lower order constructs are not part of the structural model,
  # we cannot extract their coding directly

  # where does the indexed construct appear as a measurement?
  idx <- model$mmMatrix[,2] == mm_coding[index, 1]
  #get that construct's type
  index_type <- model$mmMatrix[idx,3]
  # is it a HOC?
  is_higher_order_measurement <- startsWith(index_type, "HOC")

  if(any(is_higher_order_measurement)) {
    # cannot be sink
    return(FALSE)
  }

  # otherwise test if it never appears in source
  issink <- !any(mm_coding[index, ] %in% model$smMatrix[,1])

  return(issink)
}


#' Generates the dot code for the measurement model
#'
#' @param model the model to use
#' @param theme the theme to use
dot_component_mm <- function(model, theme) {
  sub_components_mm <- c(paste0("// ---------------------\n",
                                "// The measurement model\n",
                                "// ---------------------\n"))

  # we use mmMatrix because model$constructs does not contain HOCs
  if (is.null(model$hoc)) {
    mm_count <- length(intersect(construct_names(model$smMatrix),unique(model$mmMatrix[,1 ])))
  } else {
    mm_count <- length(intersect(unique(c(model$smMatrix, model$first_stage_model$smMatrix)),unique(model$mmMatrix[,1 ])))
  }


  for (i in 1:mm_count) {
    sub_component <- dot_subcomponent_mm(i, model, theme)
    sub_components_mm <- c(sub_components_mm, sub_component)
  }

  glue_dot(paste0(sub_components_mm, collapse = "\n"))

}

#' generates the dot code for a subgraph (per construct)
#'
#' @param index the index of the construct
#' @param model the model to use
#' @param theme the theme to use
dot_subcomponent_mm <- function(index, model, theme) {

  mm_coding <- extract_mm_coding(model)

  node_style <- get_mm_node_style(theme)

  # test-flags for component types
  is_reflective <- mm_coding[index, 2] == "C"
  is_interaction <- mm_coding[index, 2] == "interaction"
  # is_higher_order <- startsWith(mm_coding[index, 2], "HOC") # maybe we need this later?

  # debug:
  # print(mm_coding[index, ])
  # no measurement for interaction terms
  # TODO: two-stage interaction
  if (is_interaction) {
    return("")
  }

  #if (is_reflective) {
  #  edge_style <- get_mm_edge_style(theme, forward = FALSE)
  #} else {
  #  edge_style <- get_mm_edge_style(theme, forward = TRUE)
  #}

  construct_type <- get_construct_type(model, mm_coding[index, 1])
  flip <- is_sink(model, index)
  edge_style <- get_mm_edge_style(theme, construct_type, flip)

  nodes <- extract_mm_nodes(index, model, theme)
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


#' extracts the constructs and their types from the model
#'
#' @param model the model to use
extract_mm_coding <- function(model) {
  # iterate over all constructs in the mmMatrix
  constructs <- constructs_in_model(model)

  # create output matrix
  mm_coding <- matrix(nrow = length(constructs$construct_names),
                      ncol = 2,
                      data = c(constructs$construct_names, constructs$construct_types))
  colnames(mm_coding) <- c("name", "type")
  return(mm_coding)
}



# 2.1 MM-Nodes ----

#' get global measurement model node style
#'
#' @param theme the theme to use
get_mm_node_style <- function(theme) {
  glue_dot(paste0("shape = box,\n",
                  "color = <<theme$mm.node.color>>,\n",
                  "fillcolor = <<theme$mm.node.fill>>,\n",
                  "style = filled,\n",
                  "fontsize = <<theme$mm.node.label.fontsize>>,\n",
                  "fontcolor = <<theme$mm.node.label.fontcolor>>,\n",
                  "height = <<theme$mm.node.height>>,\n",
                  "width = <<theme$mm.node.width>>,\n",
                  "fontname = <<theme$plot.fontname>>,\n",
                  "fixedsize = true\n"))
}



#' gets the individual nodes and applies formatting
#'
#' @param index the index of the construct
#' @param model the model to use
#' @param theme the theme to use
extract_mm_nodes <- function(index, model, theme) {
  mm_coding <- extract_mm_coding(model)
  mm_matrix <- model$mmMatrix
  mm_matrix_subset <- mm_matrix[mm_matrix[, 1] == mm_coding[index, 1], ,drop = FALSE] # Should now always be a matrix

  shape <- get_mm_node_shape(model, mm_matrix_subset[1,1], theme)
  nodes <- paste0(
      paste0("\"",mm_matrix_subset[, 2],"\" [label = \"", mm_matrix_subset[, 2], "\"", shape, "]"),
    collapse = "\n")

  return(nodes)
}



#' Get a string to insert into a node specification using the themed shape
#'
#' @param model the model to use
#' @param construct the construct to use
#' @param theme the theme to use
#'
#' @return Returns a string that determines the shape of a node
get_mm_node_shape <- function(model, construct, theme) {
  construct_type <- get_construct_type(model, construct)

  result <- switch(construct_type,
                   "interaction" = ", shape = ellipse",
                   "C" = paste0(", shape = ", theme$manifest.reflective.shape),
                   "B" = paste0(", shape = ", theme$manifest.compositeB.shape),
                   "A" = paste0(", shape = ", theme$manifest.compositeA.shape),
                   "HOCA" = paste0(", shape = ", theme$manifest.compositeA.shape),
                   "HOCB" = paste0(", shape = ", theme$manifest.compositeB.shape),
                   "UNIT" = paste0(", shape = ", theme$construct.compositeB.shape)
  )
  return(result)
}





# 2.2 MM-Edges ----

#' individual styles for measurement model edges
#'
#' @param theme the theme to use
#' @param construct_type Forward direction?
#' @param flip invert the arrow direction because of sink?
get_mm_edge_style <- function(theme, construct_type, flip = FALSE){

  # read direction for matching construct type from theme
  if (construct_type == "C") {
    direction <- theme$construct.reflective.arrow
  }
  if (construct_type == "A" || construct_type == "HOCA") {
    direction <- theme$construct.compositeA.arrow
  }
  if (construct_type == "B" || construct_type == "HOCB" || construct_type == "UNIT") {
    direction <- theme$construct.compositeB.arrow
  }

  # flip the direction if sink
  if(flip){
    if(direction == "forward") {
      direction <- "backward"
    } else
    if(direction == "backward") {
      direction <- "forward"
    }
  }

  # generate arrows from direction
  if (direction == "forward") {
    arrowhead <- "normal"
    arrowtail <- "none"
  }
  if (direction == "backward") {
    arrowhead <- "none"
    arrowtail <- "normal"
  }
  if (direction == "none") {
    arrowhead <- "none"
    arrowtail <- "none"
  }

  if (!is.na(theme$mm.edge.minlen)) {
    minlen_str <- glue_dot("minlen = <<theme$mm.edge.minlen>>,")
  } else {
    minlen_str <- ""
  }

  glue_dot(paste0(c("color = <<theme$mm.edge.positive.color>>,", #default as fallback
                    "fontsize = <<theme$mm.edge.label.fontsize>>,",
                    "fontcolor = <<theme$mm.edge.label.fontcolor>>,",
                    "fontname = <<theme$plot.fontname>>,",
                    "<<minlen_str>>",
                    "dir = both",
                    "arrowhead = <<arrowhead>>",
                    "arrowtail = <<arrowtail>>"),
                  collapse = "\n"))
}



#' gets the mm_edge value (loading, weight) for bootstrapped and regular models
#'
#' @param model the model to use
#' @param theme the theme to use
#' @param indicator the indicator to use
#' @param construct the construct to use
extract_mm_edge_value <- function(model, theme, indicator, construct){

  use_weights <- use_construct_weights(theme,
                                       get_construct_type(model, construct))
# TODO: Redundancy in the next few lines can it be permanently deleted?
  # if ("boot_seminr_model" %in% class(model)) {
  #   boot_construct <- paste0(construct, " Boot Mean")
  #   if (use_weights) {
  #     loading <-
  #       round(model$outer_weights[indicator, boot_construct], theme$plot.rounding)
  #   } else {
  #     loading <-
  #       round(model$outer_loadings[indicator, boot_construct], theme$plot.rounding)
  #   }
  # }
  # if ("pls_model" %in% class(model)) {
    if (use_weights) {
      loading <-
        round(model$outer_weights[indicator, construct], theme$plot.rounding)
    } else {
      loading <-
        round(model$outer_loadings[indicator, construct], theme$plot.rounding)
    }
  # }
  return(loading)
}




#' Should a construct type use weights or loadings
#'
#' @param theme The theme to use
#' @param construct_type the construct type to test
#'
#' @return TRUE if weights should be used, FALSE if loadings
#' @keywords internal
#'
use_construct_weights <- function(theme, construct_type) {
  if (construct_type == "C") {
    return(theme$construct.reflective.use_weights)
  }
  if (construct_type == "A" || construct_type == "HOCA") {
    return(theme$construct.compositeA.use_weights)
  }
  if (construct_type == "B" || construct_type == "HOCB" || construct_type == "UNIT") {
    return(theme$construct.compositeB.use_weights)
  }


}


#' extract mm edges from model for a given index of all constructs
#'
#' @param index the index of the construct
#' @param model the model to use
#' @param theme the theme to use
#' @param weights a default weight for measurment models (high values suggested)
extract_mm_edges <- function(index, model, theme, weights = 1000) {

  mm_coding <- extract_mm_coding(model)
  mm_matrix <- model$mmMatrix

  # get row_index of all matching mm_matrix rows
  matching_rows <- mm_matrix[, 1] == mm_coding[index, 1]
  mm_matrix_subset <- mm_matrix[matching_rows, ,drop = FALSE]

  edges <- ""

  # determine letter to use (What is with A and B type constructs?)
  # Small mathematical lambda
  if (theme$plot.specialcharacters) {
    lambda <- "\U0001D706" # nonbold
    # lambda <- "\U0001D740" #bold
  } else {
    lambda <- "lambda"
  }

  for (i in 1:nrow(mm_matrix_subset)) {
    if (theme$plot.randomizedweights) {
      # Does this help with determinism in the layout?
      weights <- weights + stats::runif(1)
    }

    manifest_variable <- mm_matrix_subset[i, 2]
    construct_variable = mm_matrix_subset[i, 1]

    use_weights <- use_construct_weights(theme,
                                                  get_construct_type(model, construct_variable))


    # If interaction variable, we skip
    if (grepl("\\*", manifest_variable)) {
      next
    }


    letter <- lambda
    # Should I use weights?
    if (use_weights) {
      letter <- "w"
    }

    if (inherits(model, "boot_seminr_model")) {
      # bootstrapped version ---
      smry <- summary(model)
      row_index <-
        paste0(manifest_variable, "  ->  ", construct_variable)
      ltbl <- smry$bootstrapped_loadings

      use_weights <- use_construct_weights(theme,
                                           get_construct_type(model, construct_variable))
      if (use_weights) {
        ltbl <- smry$bootstrapped_weights
      }

      boot_values <-
        extract_bootstrapped_values(ltbl, row_index, model, theme)



      if (theme$mm.edge.boot.show_t_value) {
        tvalue <-
          paste0("t = ", round(boot_values[["tvalue"]], theme$plot.rounding))
      }
      if (theme$mm.edge.boot.show_p_value) {
        pvalue <- paste0("p ", pvalr(boot_values[["p"]], html = TRUE))
      }
      if (theme$mm.edge.boot.show_p_stars) {
        stars <- psignr(boot_values[["p"]], html = TRUE)
      }
      if (theme$mm.edge.boot.show_ci) {
        civalue <-
          paste0("95% CI [", boot_values[["lower"]], ", ", boot_values[["upper"]], "]")
      }
    } else {
      # non-bootstrapped version ---
      tvalue <- ""
      pvalue <- ""
      stars <- ""
      civalue <- ""
    }

    # extract_mm_edge_value gets the correct value for bootstrapped and non bootstrapped models
    loading <- extract_mm_edge_value(model, theme,
                                     indicator = manifest_variable,
                                     construct = construct_variable)

    if (theme$mm.edge.label.show) {
      edge_label <- format_edge_boot_label(theme$mm.edge.boot.template, letter,
                                           loading, tvalue, pvalue, stars, civalue)
    } else {
      edge_label <- ""
    }

    edge_style <-
      get_value_dependent_mm_edge_style(loading, theme)

    if(is_sink(model,index)) {
      source_node <- mm_matrix_subset[i, 1]
      target_node <- mm_matrix_subset[i, 2]
    } else {
      # TODO flip edges
      source_node <- mm_matrix_subset[i, 2]
      target_node <- mm_matrix_subset[i, 1]
    }

    # append edge
    edges <- paste0(
      edges,
      "\"",
      source_node,
      "\" -> {\"",
      target_node,
      "\"}",
      "[weight = ",
      weights,
      edge_label,
      ", penwidth = ",
      abs(loading * theme$mm.edge.width_multiplier) + theme$mm.edge.width_offset,
      edge_style,
      "]\n"
    )
  }


  return(edges)
}



#' Formats the style of the structural model edges
#'
#' @param value value to compare for negativity
#' @param theme the theme to use
#'
#' @return Returns the style for the edge (both style and color)
get_value_dependent_mm_edge_style <- function(value, theme){
  edge_style <- paste0(", style = ", theme$mm.edge.positive.style,
                       ", color = ", theme$mm.edge.positive.color)
  if (value < 0) {
    edge_style <- paste0(", style = ", theme$mm.edge.negative.style,
                         ", color = ", theme$mm.edge.negative.color)
  }
  edge_style
}


# END ----
# ___________________  ----
# EXPERIMENTAL STUFF ----

hyperedge <- function(){
  dot <- "digraph {
  A [label = IV]
  B [label = DV]
  C [label = Moderator]
  empty [label = '', shape = point, width = 0, height = 0]

  A -> empty  [arrowhead = none, weight = 1000, headlabel = <<BR/>test>]
  empty -> B [weight = 1000]
  C -> empty [constraint = FALSE]
  C -> B
}
"
  #DiagrammeR::grViz(dot)
}

# Font things
dot <- "digraph {
  A [label = IV]
  B [label = DV]

  A -> B [label = < <FONT POINT-SIZE='20'> <B><I>lamda</I> &nbsp; = 0.3</B></FONT> <BR />  <I>p</I> &lt; 0.001 >]
}"

#DiagrammeR::grViz(dot)
