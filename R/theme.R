# Everything related to seminr_themes

#' Create a theme for a seminr graph visualization
#'
#' @param plot.title.fontsize Font size of the title.
#' @param plot.fontname Font to be used throughout the plot.
#' @param plot.splines Whether or not to use splines as edges (default = TRUE).
#' @param plot.rounding The amount of decimals to keep for rounding (default = 3).
#' @param plot.adj TRUE or FALSE (default). Whether or not to use adjusted r^2 in constructs.
#' @param plot.specialcharacters Whether or not to use greek UTF-8 symbols in plots.
#' @param plot.randomizedweights TRUE or FALSE (default), decides whether to add.
#' minimal random weights to the measurement model. Can help with determinism in plot outcomes.
#' @param plot.bgcolor The background color of the plot (default = "transparent").
#' @param mm.node.color Color of the measurement model nodes.
#' @param mm.node.fill Fill of the measurement model nodes.
#' @param mm.node.label.fontsize Font size of the measurement model node labels.
#' @param mm.node.label.fontcolor Color of the measurement model node labels.
#' @param mm.edge.positive.color Color of the measurement model edges, when values are positive.
#' @param mm.edge.negative.color Color of the measurement model edges, when values are negative.
#' @param mm.edge.positive.style Style of the measurement model edges, when values are positive.
#' @param mm.edge.negative.style Style of the measurement model edges, when values are negative.
#' @param mm.edge.label.fontsize Font size of the measurement model edge labels.
#' @param mm.edge.label.fontcolor Font color of the measurement model edge labels.
#' @param mm.edge.minlen Minimum length of the measurement model edges.
#' @param mm.edge.width_offset The minimal width of an edge of the measurement model (default = 0.5).
#' @param mm.edge.use_outer_weights Whether or not to use outer weights as edge labels in the measurement model.
#' @param mm.edge.boot.show_t_value Should boot-strapped loadings/weights show a t-value
#' @param mm.edge.boot.show_p_value Should boot-strapped loadings/weights show a p-value
#' @param mm.edge.boot.show_p_stars Should boot-strapped loadings/weights show significance stars
#' @param mm.edge.boot.show_ci Should boot-strapped loadings/weights show a 95 percent confidence interval
#' @param mm.edge.boot.template A template string for HTML formatting of edges for loadings/weights
#' @param sm.node.color Color of the structural model nodes.
#' @param sm.node.fill Fill of the structural model nodes.
#' @param sm.node.label.fontsize Font size of the structural model node labels.
#' @param sm.node.label.fontcolor Font color of the structural model node labels.
#' @param sm.node.endo.template A template string for the nodes of endogenous constructs
#' @param sm.node.exo.template A template string for the nodes of exogenous constructs
#' @param sm.edge.boot.show_t_value Should boot-strapped path coefficients show a t-value
#' @param sm.edge.boot.show_p_value Should boot-strapped path coefficients show a p-value
#' @param sm.edge.boot.show_p_stars Should boot-strapped path coefficients show significance stars
#' @param sm.edge.boot.show_ci Should boot-strapped path coefficients show a 95 percent confidence interval
#' @param sm.edge.boot.template A template string for HTML formatting of edges
#' @param sm.edge.positive.color Color of the structural model edges, when values are positive.
#' @param sm.edge.negative.color Color of the structural model edges, when values are negative.
#' @param sm.edge.positive.style Style of the structural model edges, when values are positive.
#' @param sm.edge.negative.style Style of the structural model edges, when values are negative.
#' @param sm.edge.label.fontsize Font size of the structural model edge labels.
#' @param sm.edge.label.fontcolor Font color of the structural model edge labels.
#' @param sm.edge.minlen Minimum length of the structural model edges.
#' @param sm.edge.width_offset The minimal width of an edge of the structural model (default = 0.5).
#' @param construct.reflective.shape Dot shape of reflective constructs
#' @param construct.compositeA.shape Dot shape of composite constructs using correlation weights
#' @param construct.compositeB.shape Dot shape of composite constructs using regression weights
#' @param manifest.reflective.shape Dot shape of manifest variables of reflective constructs
#' @param manifest.compositeA.shape Dot shape of manifest variables of composite constructs using correlation weights
#' @param manifest.compositeB.shape Dot shape of manifest variables of composite constructs using regression weights
#'
#' @return A \code{seminr.theme} object that can be supplied to \code{\link{dot_graph}}
#' @export
#'
# @examples
seminr_theme_create <- function(plot.title.fontsize = 24,
                         plot.fontname = "helvetica",
                         plot.splines = TRUE,
                         plot.rounding = 3,
                         plot.adj = FALSE,
                         plot.specialcharacters = TRUE,
                         plot.randomizedweights = FALSE,
                         plot.bgcolor = "transparent",
                         mm.node.color = "dimgrey",
                         mm.node.fill = "white",
                         mm.node.label.fontsize = 8,
                         mm.node.label.fontcolor = "black",
                         mm.edge.positive.color = "dimgrey",
                         mm.edge.negative.color = "dimgrey",
                         mm.edge.positive.style = "solid",
                         mm.edge.negative.style = "dashed",
                         mm.edge.label.fontsize = 7,
                         mm.edge.label.fontcolor = "black",
                         mm.edge.minlen = 1,
                         mm.edge.width_offset = 0.5,
                         mm.edge.use_outer_weights = TRUE,
                         mm.edge.boot.show_t_value = FALSE,
                         mm.edge.boot.show_p_value = FALSE,
                         mm.edge.boot.show_p_stars = TRUE,
                         mm.edge.boot.show_ci = FALSE,
                         mm.edge.boot.template = edge_template_minimal(),
                         sm.node.color = "black",
                         sm.node.fill = "white",
                         sm.node.label.fontsize = 12,
                         sm.node.label.fontcolor = "black",
                         sm.node.endo.template = node_endo_template_default(),
                         sm.node.exo.template = node_exo_template_default(),
                         sm.edge.boot.show_t_value = FALSE,
                         sm.edge.boot.show_p_value = FALSE,
                         sm.edge.boot.show_p_stars = TRUE,
                         sm.edge.boot.show_ci = TRUE,
                         sm.edge.boot.template = edge_template_default(),
                         sm.edge.positive.color = "black",
                         sm.edge.negative.color = "black",
                         sm.edge.positive.style = "solid",
                         sm.edge.negative.style = "dashed",
                         sm.edge.label.fontsize = 9,
                         sm.edge.label.fontcolor = "black",
                         sm.edge.minlen = NA,
                         sm.edge.width_offset = 0.5,
                         construct.reflective.shape = "ellipse",
                         construct.compositeA.shape = "ellipse",
                         construct.compositeB.shape = "ellipse",
                         manifest.reflective.shape = "box",
                         manifest.compositeA.shape = "box",
                         manifest.compositeB.shape = "box") {

  # Do some sanity checks
  color_options <- grDevices::colors()

  if (grepl(" ", plot.fontname)) {
    plot.fontname <- paste0("'", plot.fontname, "'")
  }

  stopifnot("Illegal color-value. Use grDevices::colors() to find legal colors." = {
    mm.node.color %in% color_options &&
    sm.node.color %in% color_options &&
    mm.edge.positive.color %in% color_options &&
    mm.edge.negative.color %in% color_options &&
    sm.edge.positive.color %in% color_options &&
    sm.edge.negative.color %in% color_options &&
    mm.node.fill %in% color_options &&
    sm.node.fill %in% color_options
    })

  stopifnot(is.numeric(plot.rounding))
  stopifnot(is.numeric(plot.title.fontsize),
            is.numeric(mm.node.label.fontsize),
            is.numeric(sm.node.label.fontsize),
            is.numeric(mm.edge.label.fontsize),
            is.numeric(sm.edge.label.fontsize))
  stopifnot(is.logical(plot.splines))
  stopifnot(is.logical(plot.adj))
  stopifnot(is.logical(mm.edge.use_outer_weights),
            is.logical(sm.edge.boot.show_t_value),
            is.logical(sm.edge.boot.show_p_value),
            is.logical(sm.edge.boot.show_ci))

  theme <- list(plot.title = "",
                plot.title.fontsize = plot.title.fontsize,
                plot.fontname = plot.fontname,
                plot.splines = plot.splines,
                plot.rounding = plot.rounding,
                plot.adj = plot.adj,
                plot.specialcharacters = plot.specialcharacters,
                plot.randomizedweights = plot.randomizedweights,
                plot.bgcolor = plot.bgcolor,
                mm.node.color = mm.node.color,
                mm.node.fill = mm.node.fill,
                mm.node.label.fontsize = mm.node.label.fontsize,
                mm.node.label.fontcolor = mm.node.label.fontcolor,
                mm.node.height = 1,
                mm.node.width = 1,
                mm.edge.positive.color = mm.edge.positive.color,
                mm.edge.negative.color = mm.edge.negative.color,
                mm.edge.positive.style = mm.edge.positive.style,
                mm.edge.negative.style = mm.edge.negative.style,
                mm.edge.label.fontsize = mm.edge.label.fontsize,
                mm.edge.label.fontcolor = mm.edge.label.fontcolor,
                mm.edge.label.show = TRUE,
                mm.edge.width_multiplier = 3,
                mm.edge.width_offset = mm.edge.width_offset,
                mm.edge.minlen = mm.edge.minlen,
                mm.edge.use_outer_weights = mm.edge.use_outer_weights,
                mm.edge.boot.show_t_value = mm.edge.boot.show_t_value,
                mm.edge.boot.show_p_value = mm.edge.boot.show_p_value,
                mm.edge.boot.show_p_stars = mm.edge.boot.show_p_stars,
                mm.edge.boot.show_ci = mm.edge.boot.show_ci,
                mm.edge.boot.template = mm.edge.boot.template,
                sm.node.color = sm.node.color,
                sm.node.fill = sm.node.fill,
                sm.node.label.fontsize = sm.node.label.fontsize,
                sm.node.label.fontcolor = sm.node.label.fontcolor,
                sm.node.height = 1,
                sm.node.width = 1,
                sm.node.endo.template = sm.node.endo.template,
                sm.node.exo.template = sm.node.exo.template,
                sm.edge.positive.color = sm.edge.positive.color,
                sm.edge.negative.color = sm.edge.negative.color,
                sm.edge.positive.style = sm.edge.positive.style,
                sm.edge.negative.style = sm.edge.negative.style,
                sm.edge.label.fontsize = sm.edge.label.fontsize,
                sm.edge.label.fontcolor = sm.edge.label.fontcolor,
                sm.edge.label.show = TRUE,
                sm.edge.boot.show_t_value = sm.edge.boot.show_t_value,
                sm.edge.boot.show_p_value = sm.edge.boot.show_p_value,
                sm.edge.boot.show_p_stars = sm.edge.boot.show_p_stars,
                sm.edge.boot.show_ci = sm.edge.boot.show_ci,
                sm.edge.boot.template = sm.edge.boot.template,
                sm.edge.width_multiplier = 5,
                sm.edge.width_offset = 0.5,
                sm.edge.minlen = sm.edge.minlen,
                construct.reflective.shape = construct.reflective.shape,
                construct.compositeA.shape = construct.compositeA.shape,
                construct.compositeB.shape = construct.compositeB.shape,
                manifest.reflective.shape = manifest.reflective.shape,
                manifest.compositeA.shape = manifest.compositeA.shape,
                manifest.compositeB.shape = manifest.compositeB.shape)
  class(theme) <- "seminr_theme"
  return(theme)
}


#' The default template for labeling endogenous construct nodes
#'
#' @return The template string
#' @export
node_endo_template_default <- function(){
  paste0("<B>{name}</B>",
         "<BR /><FONT POINT-SIZE='10'>{rstring}</FONT>")
}


#' The default template for labeling exogenous construct nodes
#'
#' @return The template string
#' @export
node_exo_template_default <- function(){
  paste0("<B>{name}</B>")
}


#' The default template for labeling bootstrapped edges
#'
#' @return The template string
#' @export
edge_template_default <- function(){
  paste0("{variable} = {value}{stars}",
         "<BR /><FONT POINT-SIZE='7'>{civalue} {tvalue} {pvalue}",
         " </FONT>")
}

#' A minimal template for labeling bootstrapped edges that only shows the
#' bootstrapped mean value
#'
#' @return The template string
#' @export
edge_template_minimal <- function(){
  paste0("{variable} = {value}{stars}")
}


#' @export
print.seminr_theme <- function(x, ...) utils::str(x)




