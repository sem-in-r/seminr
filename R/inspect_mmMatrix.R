## Construct (mmMatrix row) level functions
construct_name <- function(construct) {
  construct[1]
}

number_of_items <- function(construct) {
  length(construct) / 3
}

construct_items <- function(construct) {
  item_indices <- seq(from=2, to=number_of_items(construct)*3 - 1, by=3)
  construct[item_indices]
}

## Public functions for manipulating mmMatrix or its rows
#' @export
as.reflective <- function (x, ...) {
  UseMethod("as.reflective", x)
}

as.reflective.measurement_model <- function(mmMatrix) {
  lapply(mmMatrix, FUN=as.reflective)
}

as.reflective.construct <- function(from) {
  reflective(construct_name(from), construct_items(from))
}
