# LIBRARIES
library(semPLS)

# FUNCTIONS
# Measurement functions
measure <- function(...) {
  return(matrix(c(...), ncol = 2, byrow = TRUE,
                dimnames = list(NULL, c("source", "target"))))
}

reflect <- function(construct_name, item_name, item_numbers, ...) {
  construct <- .construct_items(construct_name, item_name, item_numbers, ...)
  return(as.vector(rbind(construct$names, construct$items)))
}

form <- function(construct_name, item_name, item_numbers, ...) {
  construct <- .construct_items(construct_name, item_name, item_numbers, ...)
  return(as.vector(rbind(construct$items, construct$names)))
}

single_item <- function(construct_name, item_name) {
  return(c(construct_name, item_name))
}

.construct_items <- function(construct_name, item_name, item_numbers, item_prefix = NULL,
                             item_mid = NULL, item_suffix = NULL) {
  items <- paste(item_prefix, item_name, item_mid, item_numbers, item_suffix, sep = "")
  names <- rep(construct_name, length(items))
  return(list(names=names, items=items))
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


# Interactions

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

    # interaction_mm <-

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
