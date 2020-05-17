#' Specifies inter-item covariances that should be supplied to 
#' CBSEM estimation (\code{\link{estimate_cbsem}}) or 
#' CFA estimation (\code{\link{estimte_cfa}})
#'
#' @param ... One or more associations defined by \code{\link{item_errors}}
#' 
#' @examples
#' covaries <- associations(
#'   item_errors(c("a1", "a2"), c("b1", "b2")),
#'   item_errors("a3", "c3")
#' )
#' 
#' @export
associations <- function(...) {
  rbind(...)
}

#' Specifies pair of items, or sets of items, that should covary. Used to 
#' specify error covariances for \code{\link{associations}}.
#' @examples
#' item_errors(c("a1", "a2"), c("b1", "b2"))
#' @export
item_errors <- function(items_a, items_b) {
  expand.grid(items_a=items_a, items_b=items_b) -> .
  t(apply(., MARGIN=1, FUN=sort)) -> .
  unique(.)
}
