# A function to take a seminr model and return item and construct descriptives
descriptives <- function(seminr_model, na.rm = TRUE) {
  #items
  item_descriptives <- desc(seminr_model$data, na.rm = na.rm)
  item_correlations <- stats::cor(seminr_model$data)
  #constructs
  construct_descriptives <- desc(seminr_model$construct_scores, na.rm = na.rm)
  construct_correlations <- stats::cor(seminr_model$construct_scores)
  return(list(statistics = list(items = convert_to_table_output(item_descriptives),
                               constructs = convert_to_table_output(construct_descriptives)),
              correlations = list(items = convert_to_table_output(item_correlations),
                                  constructs = convert_to_table_output(construct_correlations))))
}

