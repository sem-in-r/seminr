#' Create lavaan syntax for a single construct's measurement
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

#' Create lavaan syntax for a single construct's endogenous paths
lavaan_regression <- function(outcome, smMatrix) {
  paste(outcome, "~", paste(antecedents_of(outcome, smMatrix), collapse=" + "))
}

#' Create Lavaan syntax for a single association between items
lavaan_association <- function(pair) {
  paste(pair[1], "~~", pair[2])
}

#' Create Lavaan syntax for inter-item associations
lavaan_item_associations <- function(item_associations) {
  if (is.null(item_associations)) return(NULL)
  associaxns <- apply(item_associations, MARGIN=1, FUN=lavaan_association)
  association_syntax <- paste("# Residual Covariances",
                              paste(associaxns, collapse="\n"),
                              sep="\n")
}

#' Create Lavaan syntax for entire measurement model
lavaan_mm_syntax <- function(measurement_model) {
  measurements <- lapply(measurement_model, FUN = lavaan_construct)
  paste("# Latent Variable Definitions",
        paste(measurements, collapse="\n"),
        sep="\n")
}

#' Create Lavaan syntax for entire structural model
lavaan_sm_syntax <- function(structural_model) {
  regressions <- lapply(all_endogenous(structural_model),
                        FUN=lavaan_regression,
                        smMatrix=structural_model)
  paste("# Regressions",
      paste(regressions, collapse="\n"),
      sep="\n")
}
