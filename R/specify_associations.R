#' SEMinR provides functions to specify inter-item covariances that
#' should be estimated, typically under CBSEM.
#'
#' @examples
#' covaries <- associations(
#'   item_errors(c("a1", "a2"), c("b1", "b2")),
#'   item_errors("a3", "c3")
#' )
#' @export
associations <- function(...) {
  rbind(...)
}

#' @examples
#' item_errors(c("a1", "a2"), c("b1", "b2"))
#' @export
item_errors <- function(items_a, items_b) {
  expand.grid(items_a=items_a, items_b=items_b) -> .
  t(apply(., MARGIN=1, FUN=sort)) -> .
  unique(.)
}
