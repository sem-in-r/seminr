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

#' Convert measurement model into mmMatrix
mm2matrix <- function(measurement_model) {
  recognized_constructs <- c("composite", "reflective", "higher_order_composite")
  construct_measurements <- measurement_model[names(measurement_model) %in% recognized_constructs]
  matrix(unlist(construct_measurements), ncol = 3, byrow = TRUE,
         dimnames = list(NULL, c("construct", "measurement", "type")))
}

#' Extract only interaction closures from measurement model
mm_interactions <- function(measurement_model) {
  measurement_model[(substr(names(measurement_model), nchar(names(measurement_model))-10, nchar(names(measurement_model))) == "interaction")]
}
