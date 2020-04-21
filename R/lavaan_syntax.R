lavaan_construct <- function(construct) {
  if (!("reflective" %in% class(construct)))
    stop(paste(construct_name(construct), "must be a reflective construct for a CBSEM model"))

  items <- construct_items(construct)
  items_syntax <- paste(items, collapse=' + ')
  measurement <- paste(construct_name(construct), "=~", items_syntax)

  extras <- NULL
  # constrain error for single item constructs
  if(length(items) == 1) {
    extras <- append(extras, paste(items, "~~", paste("0*", items, sep="")))
  }

  paste(c(measurement, extras), collapse="\n")
}

lavaan_regression <- function(outcome, smMatrix) {
  paste(outcome, "~", paste(antecedents(outcome, smMatrix), collapse=" + "))
}

lavaan_regressions <- function(smMatrix) {
  outcomes <- unique(smMatrix[, "target"])
  regressions <- lapply(outcomes, FUN=lavaan_regression, smMatrix=smMatrix)
}

lavaan_association <- function(pair) {
  paste(pair[1], "~~", pair[2])
}
