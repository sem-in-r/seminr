## Construct (mmMatrix row) level functions
number_of_items <- function(construct) {
  length(construct) / 3
}

construct_name <- function(construct) {
  construct[1]
}

all_construct_names <- function(mmMatrix) {
  lapply(mmMatrix, FUN=construct_name) -> .
  unlist(., use.names = FALSE)
}

construct_items <- function(construct) {
  item_indices <- seq(from=2, to=number_of_items(construct)*3 - 1, by=3)
  construct[item_indices]
}

all_items <- function(mmMatrix) {
  sapply(mmMatrix, FUN=construct_items) -> .
  unlist(., use.names = FALSE) -> .
  unique(.)
}

## Public functions for manipulating mmMatrix or its rows
#' @export
as.reflective <- function (x, ...) {
  UseMethod("as.reflective", x)
}

#' @export
as.reflective.measurement_model <- function(mmMatrix) {
  lapply(mmMatrix, FUN=as.reflective)
}

#' @export
as.reflective.construct <- function(from) {
  reflective(construct_name(from), construct_items(from))
}
