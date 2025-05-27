# PURPOSE: inspect and extract information from measurement models
# and mmMatrix.

# Construct (mmMatrix row) level functions
# Return indicators of a construct from mmMatrix
construct_indicators <- function(construct_name, mmMatrix) {
  mmMatrix[mmMatrix[,1] == construct_name, 2]
}

# get number of items from a construct in a measurement model
number_of_items <- function(construct) {
  length(construct) / 3
}

# Get name of construct from a measurement model
construct_name <- function(construct) {
  construct[1]
}

# Get all reflective constructs from mmMatrix that are included in the STRUCTURAL MODEL
all_reflective <- function(mmMatrix, constructs) {
  unique(mmMatrix[mmMatrix[, "type"]=="C", "construct"])
}

# Get names of all constructs in a measurement model
all_construct_names <- function(measurement_model) {
  constructs_only <- mm_constructs(measurement_model)
  lapply(constructs_only, FUN=construct_name) -> .
  unlist(., use.names = FALSE)
}

# Get names of all items of a construct in a measurement model
construct_items <- function(construct) {
  item_indices <- seq(from=2, to=number_of_items(construct)*3 - 1, by=3)
  construct[item_indices]
}

# Get names of all items from measurement model
all_items <- function(measurement_model) {
  constructs_only <- mm_constructs(measurement_model)
  sapply(constructs_only, FUN=construct_items) -> .
  unlist(., use.names = FALSE) -> .
  unique(.)
}

all_loc_non_int_items <- function(measurement_model) {
  loc_constructs_only <- loc_constructs(measurement_model)
  constructs_only <- mm_constructs(loc_constructs_only)
  sapply(constructs_only, FUN=construct_items) -> .
  unlist(., use.names = FALSE) -> .
  unique(.)
}

## Public functions for manipulating mmMatrix or its rows

#' Converts all contructs of a measurement model, or just a single construct
#'  into reflective factors.
#'
#' @param x A measurement model defined by \code{\link{constructs}}
#'   or a single composite construct defined by \code{\link{composite}}
#'
#' @param ... Any further parameters for the specific construct.
#'
#' @return A list of reflective constructs.
#' @examples
#'
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Value",        multi_items("PERV", 1:2))
#' )
#'
#' new_mm <- as.reflective(mobi_mm)
#'
#' @seealso \code{\link{as.reflective.measurement_model}},
#'   \code{\link{as.reflective.construct}}
#'
#' @export
as.reflective <- function (x, ...) {
  UseMethod("as.reflective", x)
}

#' Converts all contructs of a measurement model, or just a single construct
#'  into reflective factors.
#'
#' @param x A measurement model defined by \code{\link{constructs}}
#'   or a single composite construct defined by \code{\link{composite}}
#'
#' @param ... Any further parameters for the specific construct.
#'
#' @return A list of reflective constructs.
#'
#' @examples
#'
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Value",        multi_items("PERV", 1:2))
#' )
#'
#' new_mm <- as.reflective(mobi_mm)
#'
#' @seealso \code{\link{as.reflective.construct}}
#'
#' @export
as.reflective.measurement_model <- function(x, ...) {
  reflectives <- lapply(x, FUN=as.reflective)
  # Filter(Negate(is.null), reflectives)
}

#' Converts a contruct of a measurement model into a reflective factor.
#'
#' @param x A measurement model defined by \code{\link{constructs}}
#'   or a single composite construct defined by \code{\link{composite}}
#'
#' @param ... Any further parameters for the specific construct.
#'
#' @return A list of reflective constructs.
#' @examples
#'
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Value",        multi_items("PERV", 1:2))
#' )
#'
#' new_mm <- as.reflective(mobi_mm)
#'
#' @seealso \code{\link{as.reflective.measurement_model}}
#'
#' @export
as.reflective.construct <- function(x, ...) {
  reflective(construct_name(x), construct_items(x))
}

#' Converts interaction of a measurement model
#'  into a reflective factors.
#'
#' @param x A measurement model defined by \code{\link{constructs}}
#'   or a single composite construct defined by \code{\link{composite}}
#'
#' @param ... Any further parameters for the specific construct.
#'
#' @return A list of reflective constructs.
#' @examples
#'
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Value",        multi_items("PERV", 1:2))
#' )
#'
#' new_mm <- as.reflective(mobi_mm)
#'
#' @seealso \code{\link{as.reflective.measurement_model}}
#'
#' @export
as.reflective.interaction <- function(x, ...) {
  x
}
#' @export
as.reflective.matrix <- function(x, ...) {
  # TODO: give interaction mmMatrix column names so we can do: from[, "type"]
  x[, 3] <- "C"
  x
}

# Convert measurement model into mmMatrix
# - if measurement model is a matrix, return it directly (used in 2-stage)
mm2matrix <- function(measurement_model) {
  if ("mmMatrix" %in% class(measurement_model)) {
    return(measurement_model)
  }

  recognized_constructs <- c("composite", "reflective", "higher_order_composite", "higher_order_reflective")
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

loc_constructs <- function(measurement_model) {
  Filter(function(e) {!("higher_order_composite" %in% class(e))}, measurement_model)
}

# Extract only interaction closures from measurement model
mm_interactions <- function(measurement_model) {
   Filter(function(e) {"interaction" %in% class(e)}, measurement_model)
}
