warning_single_item_formative <- function(mmMatrix) {
  constructs <- all_constructs(mmMatrix)
  for(construct in constructs) {
    if(is_single_item(mmMatrix, construct) && is_LOC_B(mmMatrix, construct)) {
      stop("You cannot define a single item construct as mode B")
    }
  }
}

warning_missing_data <- function(data, mmMatrix) {
  non_hoc_constructs <- all_LOC(mmMatrix)
  mm_items <- unlist(sapply(non_hoc_constructs,
    function(c) construct_items(mmMatrix, c), USE.NAMES = FALSE))
  mm_items <- mm_items[!is_interaction(mm_items)]
  data <- data[, mm_items]
  N <- nrow(data)
  missing_values <- which(stats::complete.cases(data)==FALSE)
  if(length(missing_values)==0){
    message("All ", N ," observations are valid.")
  }
  else {
    message("Data rows ", paste(missing_values, collapse=", "),
            " contain missing values and will be omitted.\n",
            "Total number of complete cases: ", N-length(missing_values))
  }
}
warnings <- function(mmMatrix,data, smMatrix) {
  warning_single_item_formative(mmMatrix)
  warning_missing_data(data, mmMatrix)
}

