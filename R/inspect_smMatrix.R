construct_names <- function(smMatrix) {
  unique(c(smMatrix[,1], smMatrix[,2]))
}

antecedents <- function(outcome, smMatrix) {
  subset_sm <- smMatrix[smMatrix[, "target"] == outcome, ]
  if (is.null(dim(subset_sm))) subset_sm <- t(subset_sm)
  subset_sm[, "source"]
}
