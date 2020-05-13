## Construct (mmMatrix row) level functions
number_of_items <- function(construct) {
  length(construct) / 3
}

construct_name <- function(construct) {
  construct[1]
}

all_construct_names <- function(measurement_model) {
  constructs_only <- mm_constructs(measurement_model)
  lapply(constructs_only, FUN=construct_name) -> .
  unlist(., use.names = FALSE)
}

construct_items <- function(construct) {
  item_indices <- seq(from=2, to=number_of_items(construct)*3 - 1, by=3)
  construct[item_indices]
}

all_items <- function(measurement_model) {
  constructs_only <- mm_constructs(measurement_model)
  sapply(constructs_only, FUN=construct_items) -> .
  unlist(., use.names = FALSE) -> .
  unique(.)
}

## Public functions for manipulating mmMatrix or its rows
#' @export
as.reflective <- function (x, ...) {
  UseMethod("as.reflective", x)
}

#' @export
as.reflective.measurement_model <- function(measurement_model) {
  reflectives <- lapply(measurement_model, FUN=as.reflective)
  # Filter(Negate(is.null), reflectives)
}

#' @export
as.reflective.construct <- function(from) {
  reflective(construct_name(from), construct_items(from))
}

#' @export
as.reflective.interaction <- function(from) {
  from
}

#' @export
as.reflective.matrix <- function(from) {
  # TODO: make an 'mmMatrix' class name
  # TODO: give interaction mmMAtrix column names so we can do: from[, "type"]
  from[, 3] <- "C"
  from
}

#' Convert measurement model into mmMatrix
#' - if measurement model is a matrix, return it directly
mm2matrix <- function(measurement_model) {
  if ("mmMatrix" %in% class(measurement_model)) {
    return(measurement_model)
  }

  recognized_constructs <- c("composite", "reflective", "higher_order_composite")
  construct_measurements <- measurement_model[names(measurement_model) %in% recognized_constructs]
  mmMatrix <- matrix(
    unlist(construct_measurements), ncol = 3, byrow = TRUE,
    dimnames = list(NULL, c("construct", "measurement", "type"))
  )

  class(mmMatrix) <- c(class(mmMatrix), "mmMatrix")
  mmMatrix
}

mm_constructs <- function(measurement_model) {
  Filter(function(e) {!("interaction" %in% class(e))}, measurement_model)
}

#' Extract only interaction closures from measurement model
mm_interactions <- function(measurement_model) {
  # TODO: refactor to?
  #  Filter(function(e) {"interaction" %in% class(e)}, measurement_model)
  measurement_model[(substr(names(measurement_model), nchar(names(measurement_model))-10, nchar(names(measurement_model))) == "interaction")]
}
