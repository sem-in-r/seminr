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

# Creates list of measurement items using root name, numbers, and affixes
#
# arguments:
#   item_name: root name of all items
#   item_numbers: vector of item numbers
#   ...: optional affix arguments
#     prefix: prefix before each item name
#     mid: insert between item name and numbers
#     suffix: suffix after each ite name
#
# e.g.> multi_items("item", 0:3, prefix="X_", mid=".", suffix="_")
#
multi_items <- function(item_name, item_numbers, ...) {
  affix <- as.data.frame(list(...))
  paste(affix$prefix, item_name, affix$mid, item_numbers, affix$suffix, sep = "")
}

single_item <- function(item) {
  return(item)
}

# Interaction Functions
# Create interaction measurement items by multipying all combination of factor items
#
# e.g. create two new interactions: Image.Expectation and Image.Value
#
# interact( interaction_combo("Image", "Expectation"),
#           interaction_combo("Image", "Value")
# )
#

interact <- function(...) {
  function(data, mm, all_intxns=list(...)) {
    create_interaction <- function(intxn_function) { intxn_function(data, mm) }
    intxns_list <- lapply(all_intxns, create_interaction)
    return(intxns_list)
  }
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

# Structural functions
paths <- function(from, to) {
  return(as.vector(t(as.matrix(expand.grid(from, to)))))
}

structure <- function(...) {
  return(matrix(c(...), ncol = 2, byrow = TRUE,
                dimnames = list(NULL, c("source", "target"))))
}

# Model Assembly Functions
modelr <- function(data, measurement_model, interactions=NULL, structural_model) {
  if(!is.null(interactions)) {
    # update data with new iteraction items
    intxns_list <- interactions(data, measurement_model)
    get_data <- function(intxn) { intxn$data }
    interaction_data <- do.call("cbind", lapply(intxns_list, get_data))
    data <- cbind(data, interaction_data)

    # update measurement model with
    measure_interaction <- function(intxn) {
      reflect(intxn$name, names(intxn$data))
    }
    intxns_mm <- measure(do.call("c", lapply(intxns_list, measure_interaction)))
    measurement_model <- rbind(measurement_model, intxns_mm)
  }

  return(list(
    model = semPLS::plsm(data = data, strucmod = structural_model, measuremod = measurement_model),
    data = data))
}


# Report Functions

plot_scores <- function(fitted_model) {
  plot(as.data.frame(fitted_model$factor_scores), pch = 16,
       col = rgb(0.5, 0.5, 0.5, alpha = 0.6))
}

