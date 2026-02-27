# PURPOSE: inspect and extract information from measurement models
# and mmMatrix.

# S3 generic: get item names from various model objects
construct_items <- function(x, ...) {
  UseMethod("construct_items")
}

# mmMatrix: items for a specific construct (container-first)
construct_items.mmMatrix <- function(x, construct_name, ...) {
  x[x[, "construct"] == construct_name, "measurement"]
}

# Plain matrix fallback (mmMatrix after rbind loses "mmMatrix" class)
construct_items.matrix <- function(x, construct_name, ...) {
  x[x[, "construct"] == construct_name, "measurement"]
}

# Construct vector: items from a construct specification
construct_items.construct <- function(x, ...) {
  item_indices <- seq(from=2, to=item_count(x)*3 - 1, by=3)
  x[item_indices]
}

# Estimated model: items for a construct (via mmMatrix)
construct_items.seminr_model <- function(x, construct_name, ...) {
  construct_items(x$mmMatrix, construct_name)
}

# Measurement model list: all item names across all constructs
construct_items.measurement_model <- function(x, ...) {
  constructs_only <- all_non_interactions(x)
  sapply(constructs_only, FUN=construct_items) -> .
  unlist(., use.names = FALSE) -> .
  unique(.)
}

# List fallback (measurement_model after append() loses class)
construct_items.list <- function(x, ...) {
  constructs_only <- all_non_interactions(x)
  sapply(constructs_only, FUN=construct_items) -> .
  unlist(., use.names = FALSE) -> .
  unique(.)
}

# get number of items from a construct in a measurement model
item_count <- function(construct) {
  length(construct) / 3
}

# Get name of construct from a measurement model
construct_name <- function(construct) {
  construct[1]
}

# Get measurement mode of a construct (first item)
construct_mode <- function(mmMatrix, construct) {
  as.matrix(mmMatrix[mmMatrix[,"construct"]==construct,"type"])[1]
}

# Get measurement mode of a construct as a function
construct_mode_fn <- function(mmMatrix, construct) {
  mode <- construct_mode(mmMatrix, construct)
  if(mode %in% c("A", "C", "HOCA")) {
    return(mode_A)
  } else if(mode %in% c("B", "HOCB")) {
    return(mode_B)
  } else if(mode == "UNIT") {
    return(unit_weights)
  }
}

# Get all unique construct names from mmMatrix
all_constructs <- function(mmMatrix) {
  unique(mmMatrix[, "construct"])
}

# Get all constructs matching a given estimation mode from mmMatrix
all_constructs_of_mode <- function(mmMatrix, mode) {
  unique(mmMatrix[mmMatrix[, "type"] == mode, "construct"])
}

# Reverse lookup: find the construct containing a given item
construct_of_item <- function(mmMatrix, item) {
  unname(mmMatrix[mmMatrix[, "measurement"] == item, "construct"][1])
}

# Check if all indicator names in a measurement model exist in the data columns
are_indicators_in_data <- function(measurement_model,
                                        data) {
  return(all(construct_items(measurement_model) %in% colnames(data)))
}

# Get all reflective constructs from mmMatrix that are included in the STRUCTURAL MODEL
all_reflective <- function(mmMatrix, constructs) {
  unique(mmMatrix[mmMatrix[, "type"]=="C", "construct"])
}

all_LOC_items <- function(measurement_model) {
  all_LOCs_only <- all_LOCs(measurement_model)
  constructs_only <- all_non_interactions(all_LOCs_only)
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
  class(reflectives) <- class(x)
  reflectives
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
  if (is.null(colnames(x))) {
    colnames(x) <- c("construct", "measurement", "type")
  }
  x[, "type"] <- "C"
  x
}

# Append rows to mmMatrix, preserving "mmMatrix" class
append_mm_rows <- function(mmMatrix, new_rows) {
  result <- rbind(mmMatrix, new_rows)
  if (!("mmMatrix" %in% class(result)) && "mmMatrix" %in% class(mmMatrix)) {
    class(result) <- class(mmMatrix)
  }
  result
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

all_non_interactions <- function(measurement_model) {
  Filter(function(e) {!("interaction" %in% class(e))}, measurement_model)
}

all_LOCs <- function(measurement_model) {
  Filter(function(e) {!("higher_order_composite" %in% class(e))}, measurement_model)
}

# Extract only interaction closures from measurement model
all_interaction_fns <- function(measurement_model) {
   Filter(function(e) {"interaction" %in% class(e)}, measurement_model)
}
