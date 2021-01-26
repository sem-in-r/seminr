

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




# alternate Smart theme ----
#' A colored theme
#' @rdname seminr_theme_default
#' @export
seminr_theme_smart <- function(plot.title.fontsize = 24,
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


#' The theme function for a modern approach of visualizing PLS models in b/w
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

  # TODO: remove arrows for composite
  # Check literature and add reference if available
  seminr_theme_create(plot.title.fontsize = plot.title.fontsize,
                      mm.node.label.fontsize = mm.node.label.fontsize,
                      sm.node.label.fontsize = sm.node.label.fontsize,
                      mm.edge.label.fontsize = mm.edge.label.fontsize,
                      sm.edge.label.fontsize = sm.edge.label.fontsize,
                      construct.reflective.shape = "ellipse",
                      construct.compositeA.shape = "hexagon",
                      construct.compositeB.shape = "ellipse"
  )
}


