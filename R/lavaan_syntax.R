# Makes sure construct name is valid for lavaan, or else transforms it
lavaanify_name <- function(name) {
  # process interaction names
  gsub("\\*", "_x_", name)
}

# Renames lavaan construct names for reporting
unlavaanify_name <- function(name) {
  # process interaction names
  gsub("_x_", "\\*", name)
}

# Create lavaan syntax for a single construct's measurement
lavaan_construct <- function(construct, mmMatrix) {
  lav_name <- lavaanify_name(construct)

  if (construct_mode(mmMatrix, construct) != "C")
    stop(paste(lav_name, "must be a reflective construct for a CBSEM model"))

  items <- lavaanify_name(construct_items(mmMatrix, construct))
  items_syntax <- paste(items, collapse=' + ')
  measurement <- paste(lav_name, "=~", items_syntax)

  extras <- NULL
  # constrain error for single item constructs
  if (length(items) == 1) {
    extras <- append(extras, paste(items, "~~", paste("0*", items, sep="")))
  }

  paste(c(measurement, extras), collapse="\n")
}

# Create lavaan syntax for a single construct's endogenous paths
lavaan_regression <- function(outcome, smMatrix) {
  lav_outcome <- lavaanify_name(outcome)
  lav_antecedents <- {
    construct_antecedents(smMatrix, outcome) -> .
    sapply(., FUN=lavaanify_name, USE.NAMES = FALSE)
  }
  paste(lav_outcome, "~", paste(lav_antecedents, collapse=" + "))
}

# Create Lavaan syntax for a single association between items
lavaan_association <- function(pair) {
  paste(pair[1], "~~", pair[2])
}

# Create Lavaan syntax for inter-item associations
lavaan_item_associations <- function(item_associations) {
  if (is.null(item_associations)) return(NULL)
  associaxns <- apply(item_associations, MARGIN=1, FUN=lavaan_association)
  association_syntax <- paste("# Residual Covariances",
                              paste(associaxns, collapse="\n"),
                              sep="\n")
}

# Create Lavaan syntax for entire measurement model
lavaan_mm_syntax <- function(mmMatrix) {
  constructs <- all_constructs(mmMatrix)
  measurements <- lapply(constructs, FUN = function(construct) {
    lavaan_construct(construct, mmMatrix)
  })
  # measurements <- lapply(constructs, FUN = lavaan_construct)
  paste("# Latent Variable Definitions",
        paste(measurements, collapse="\n"),
        sep="\n")
}

# Create Lavaan syntax for entire structural model
lavaan_sm_syntax <- function(smMatrix) {
  regressions <- lapply(all_endogenous(smMatrix),
                        FUN=lavaan_regression,
                        smMatrix=smMatrix)
  paste("# Regressions",
      paste(regressions, collapse="\n"),
      sep="\n")
}
