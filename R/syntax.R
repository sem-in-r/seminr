# LIBRARIES
library(semPLS)

# FUNCTIONS
# Measurement functions
measure <- function(...) {
  return(matrix(c(...), ncol = 2, byrow = TRUE,
                dimnames = list(NULL, c("source", "target"))))
}

reflect <- function(construct_name, item_names) {
  construct_names <- rep(construct_name, length(item_names))
  return(c(rbind(construct_names, item_names)))
}

form <- function(construct_name, item_names) {
  construct_names <- rep(construct_name, length(item_names))
  return(c(rbind(item_names, construct_names)))
}

multi_items <- function(item_name, item_numbers,
                       item_prefix = NULL, item_mid = NULL, item_suffix = NULL) {
  paste(item_prefix, item_name, item_mid, item_numbers, item_suffix, sep = "")
}

# Structural functions
paths <- function(from, to) {
  return(as.vector(t(as.matrix(expand.grid(from, to)))))
}

structure <- function(...) {
  return(matrix(c(...), ncol = 2, byrow = TRUE,
                dimnames = list(NULL, c("source", "target"))))
}

plot_scores <- function(fitted_model) {
  plot(as.data.frame(fitted_model$factor_scores), pch = 16,
       col = rgb(0.5, 0.5, 0.5, alpha = 0.6))
}


# Interaction Functions

# Create new interaction data items by multipying all combination of factor items
#
# e.g.> interaction_combo(mobi, mobi_mm, "Image", "Expectation")
#

interact <- function(data, mm, ...) {
  create_interaction <- function(intxn_function) { intxn_function(data, mm) }
  interactions <- lapply(list(...), create_interaction)
  return(interactions)
}

interaction_combo <- function(factor1, factor2) {
  function(data, mm) {
    interaction_name <- paste(factor1, factor2, sep=".")
    iv1_items <- mm[mm[, "source"] == factor1, ][, "target"]
    iv2_items <- mm[mm[, "source"] == factor2, ][, "target"]

    iv1_data <- data[iv1_items]
    iv2_data <- data[iv2_items]

    mult <- function(col) {
      iv2_data*col
    }

    multiples_list <- lapply(iv1_data, mult)
    interaction_data <- do.call("cbind", multiples_list)

    return(list(name = interaction_name, data = interaction_data))
  }
}

# Model Assembly Functions
modelr <- function(data, measurement_model, structural_model, interactions=NULL) {
  if(!is.null(interactions)) {
    # update data with new iteraction items
    get_data <- function(intxn) { intxn$data }
    interaction_data <- do.call("cbind", lapply(interactions, get_data))
    data <- cbind(data, interaction_data)

    # update measurement model with
    create_interaction <- function(intxn) {
      reflect(intxn$name, names(intxn$data))
    }
    interactions_mm <- measure(do.call("c", lapply(interactions, create_interaction)))
    measurement_model <- rbind(measurement_model, interactions_mm)
  }

  return(list(
    model = semPLS::plsm(data = data, strucmod = structural_model, measuremod = measurement_model),
    data = data))
}
