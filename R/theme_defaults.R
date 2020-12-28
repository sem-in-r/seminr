

#' The theme function for a basic b/w theme
#'
#' @param plot.title.fontsize Title font size
#' @param sm.node.label.fontsize Font size for constructs
#' @param mm.node.label.fontsize Font size for measurement variables
#' @param sm.edge.label.fontsize Font size for path edges
#' @param mm.edge.label.fontsize  Font size for measurement model edges
#'
#' @return a theme object
#' @export
#'
# @examples
seminr_theme_default <- function(plot.title.fontsize = 24,
                                 mm.node.label.fontsize = 8,
                                 sm.node.label.fontsize = 12,
                                 mm.edge.label.fontsize = 7,
                                 sm.edge.label.fontsize = 9
){

  seminr_theme_create(plot.title.fontsize = plot.title.fontsize,
               mm.node.label.fontsize = mm.node.label.fontsize,
               sm.node.label.fontsize = sm.node.label.fontsize,
               mm.edge.label.fontsize = mm.edge.label.fontsize,
               sm.edge.label.fontsize = sm.edge.label.fontsize
               )
}




# alternate SmartPLS theme ----
#' A SmartPLS-based theme
#' @rdname seminr_theme_default
#' @export
seminr_theme_smartpls <- function(plot.title.fontsize = 24,
                                  mm.node.label.fontsize = 8,
                                  sm.node.label.fontsize = 12,
                                  mm.edge.label.fontsize = 7,
                                  sm.edge.label.fontsize = 9
){

  seminr_theme_create(plot.title.fontsize = plot.title.fontsize,
               mm.node.label.fontsize = mm.node.label.fontsize,
               sm.node.label.fontsize = sm.node.label.fontsize,
               mm.edge.label.fontsize = mm.edge.label.fontsize,
               sm.edge.label.fontsize = sm.edge.label.fontsize,
               sm.node.fill = "lightcyan",
               mm.node.fill = "lightgoldenrodyellow"
  )
}
