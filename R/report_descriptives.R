# A function to take a seminr model and return item and construct descriptives
descriptives <- function(seminr_model, na.rm = TRUE) {
  #items
  item_descriptives <- desc(seminr_model$data, na.rm = na.rm)
  item_correlations <- cor(seminr_model$data)
  #constructs
  construct_descriptives <- desc(seminr_model$construct_scores, na.rm = na.rm)
  construct_correlations <- cor(seminr_model$construct_scores)
  class(item_descriptives) <- append(class(item_descriptives), "table_output")
  class(item_correlations) <- append(class(item_correlations), "table_output")
  class(construct_descriptives) <- append(class(construct_descriptives), "table_output")
  class(construct_correlations) <- append(class(construct_correlations), "table_output")
  return(list(statistics = list(items = item_descriptives,
                               constructs = construct_descriptives),
              correlations = list(items = item_correlations,
                                  constructs = construct_correlations)))
}

