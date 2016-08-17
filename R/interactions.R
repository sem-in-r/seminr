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
