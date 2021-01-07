# Everything related to seminr_themes

#' Create a theme for a seminr graph visualization
#'
#' @param plot.title.fontsize Font size of the title.
#' @param plot.fontname Font to be used throughout the plot.
#' @param plot.splines Whether or not to use splines as edges.
#' @param plot.rounding The amount of decimals to keep for rounding.
#' @param plot.adj Whether or not to use adjusted r^2 in constructs
#' @param mm.node.color Color of the measurement model nodes.
#' @param mm.node.fill Fill of the measurement model nodes.
#' @param mm.node.label.fontsize Font size of the measurement model node labels.
#' @param mm.edge.color Color of the measurement model edges.
#' @param mm.edge.label.fontsize Font size of the measurement model edge labels.
#' @param mm.edge.minlen Minimum length of the measurement model edges.
#' @param mm.edge.use_outer_weights Whether or not to use outer weights as edge labels in the measurement model.
#' @param sm.node.color Color of the structural model nodes.
#' @param sm.node.fill Fill of the structural model nodes.
#' @param sm.node.label.fontsize Font size of the structural model node labels.
#' @param sm.edge.boot.show_t_value Should boot-strapped path coefficients show a t-value
#' @param sm.edge.boot.show_p_value Should boot-strapped path coefficients show a p-value
#' @param sm.edge.boot.show_ci Should boot-strapped path coefficients show a 95 percent confidence interval
#' @param sm.edge.boot.template A template string for HTML formatting of edges
#' @param sm.edge.color Color of the structural model edges.
#' @param sm.edge.label.fontsize Font size of the structural model edge labels.
#' @param sm.edge.minlen Minimum length of the structural model edges.
#'
#' @return A \code{seminr.theme} object that can be supplied to \code{\link{dot_graph}}
#' @export
#'
# @examples
seminr_theme_create <- function(plot.title.fontsize = 24,
                         plot.fontname = "helvetica",
                         plot.splines = TRUE,
                         plot.rounding = 3,
                         plot.adj = TRUE,
                         mm.node.color = "dimgrey",
                         mm.node.fill = "white",
                         mm.node.label.fontsize = 8,
                         mm.edge.color = "dimgrey",
                         mm.edge.label.fontsize = 7,
                         mm.edge.minlen = 1,
                         mm.edge.use_outer_weights = TRUE,
                         sm.node.color = "black",
                         sm.node.fill = "white",
                         sm.node.label.fontsize = 12,
                         sm.edge.boot.show_t_value = FALSE,
                         sm.edge.boot.show_p_value = TRUE,
                         sm.edge.boot.show_ci = FALSE,
                         sm.edge.boot.template = "<B>{variable} = {value}</B><BR /><FONT POINT-SIZE='7'>{tvalue} - {pvalue}<BR/>{civalue}</FONT>",
                         sm.edge.color = "black",
                         sm.edge.label.fontsize = 9,
                         sm.edge.minlen = NA) {

  # Do some sanity checks
  color_options <- grDevices::colors()

  if (grepl(" ", plot.fontname)) {
    plot.fontname <- paste0("'", plot.fontname, "'")
  }

  stopifnot("Illegal color-value. Use grDevices::colors() to find legal colors." = {
    mm.node.color %in% color_options &&
    sm.node.color %in% color_options &&
    mm.edge.color %in% color_options &&
    sm.edge.color %in% color_options &&
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
                mm.node.color = mm.node.color,
                mm.node.fill = mm.node.fill,
                mm.node.label.fontsize = mm.node.label.fontsize,
                mm.node.height = 1,
                mm.node.width = 1,
                mm.edge.color = mm.edge.color,
                mm.edge.label.fontsize = mm.edge.label.fontsize,
                mm.edge.label.show = TRUE,
                mm.edge.width_multiplier = 3,
                mm.edge.minlen = mm.edge.minlen,
                mm.edge.use_outer_weights = mm.edge.use_outer_weights,
                sm.node.color = sm.node.color,
                sm.node.fill = sm.node.fill,
                sm.node.label.fontsize = sm.node.label.fontsize,
                sm.node.height = 1,
                sm.node.width = 1,
                sm.edge.color = sm.edge.color,
                sm.edge.label.fontsize = sm.edge.label.fontsize,
                sm.edge.label.show = TRUE,
                sm.edge.boot.show_t_value = sm.edge.boot.show_t_value,
                sm.edge.boot.show_p_value = sm.edge.boot.show_p_value,
                sm.edge.boot.show_ci = sm.edge.boot.show_ci,
                sm.edge.boot.template = sm.edge.boot.template,
                sm.edge.width_multiplier = 5,
                sm.edge.minlen = sm.edge.minlen)
  class(theme) <- "seminr.theme"
  return(theme)
}





#' @export
print.seminr_theme <- function(x, ...) utils::str(x)

#' Reports whether x is a seminr_theme object
#' @param x An object to test
#' @export
#' @keywords internal
is.seminr_theme <- function(x) inherits(x, "seminr_theme")




