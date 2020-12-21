#' The theme function for a basic b/w theme
#'
#' @param title_fontsize Title font size
#' @param construct_fontsize Font size for constructs
#' @param item_fontsize Font size for measurement variables
#' @param path_fontsize Font size for path edges
#' @param mm_fontsize  Font size for measurement model
#'
#' @return the basic theme object
#' @export
#'
# @examples
seminr_theme_default <-
  function(title_fontsize = 24,
           construct_fontsize = 14,
           item_fontsize = 8,
           path_fontsize = 12,
           mm_fontsize = 8) {

    new_seminr_theme(
      title_font = "helvetica",
      title_size = title_fontsize,
      splines = TRUE,
      rounding = 3,
      adjusted = TRUE,
      # Construct
      construct_nodes = seminr_theme_node(
        shape = "ellipse",
        color = "black",
        fill = "white",
        fontsize = construct_fontsize
      ),
      # items
      item_nodes = new_seminr_theme_node(
        shape = "box",
        fill = "white",
        color = "black",
        fontsize = item_fontsize
      ),
      #edges
      outer_edges = new_seminr_theme_edge(color = "dimgray", fontsize = mm_fontsize),
      inner_edges = new_seminr_theme_edge(fontsize = path_fontsize)
    )
  }



# alternate SmartPLS theme ----

#' A SmartPLS inspired theme
#'
#' @param title_fontsize Title font size
#' @param construct_fontsize Font size for constructs
#' @param item_fontsize Font size for measurement variables
#' @param path_fontsize Font size for path edges
#' @param mm_fontsize  Font size for measurement model
#'
#' @return the theme object
#' @export
#'
# @examples
seminr_theme_smartpls <- function(title_fontsize = 24,
                                  construct_fontsize = 14,
                                  item_fontsize = 8,
                                  path_fontsize = 12,
                                  mm_fontsize = 8
){
  new_seminr_theme(
    title_font = "helvetica",
    title_size = title_fontsize,
    splines = TRUE,
    rounding = 3,
    adjusted = TRUE,
    # Constructs
    construct_nodes = new_seminr_theme_node(
      shape = "oval",
      color = "black",
      fill = "lightcyan",
      fontsize = construct_fontsize
    ),
    # Items
    item_nodes = new_seminr_theme_node(
      shape = "box",
      fill = "lightgoldenrodyellow",
      color = "black",
      fontsize = item_fontsize
    ),
    # edges
    outer_edges = new_seminr_theme_edge(color = "dimgray", fontsize = mm_fontsize),
    inner_edges = new_seminr_theme_edge(fontsize = path_fontsize)
  )
}
