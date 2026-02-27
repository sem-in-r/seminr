warning_single_item_formative <- function(mmMatrix) {
  constructs <- all_constructs(mmMatrix)
  for(construct in constructs) {
    if(length(construct_indicators(construct, mmMatrix)) == 1 && measure_mode(construct, mmMatrix) == "B") {
      stop("You cannot define a single item construct as mode B")
    }
  }
}

warning_missing_data <- function(data, mmMatrix) {
  non_hoc_constructs <- setdiff(all_constructs(mmMatrix),
    c(all_constructs_of_mode(mmMatrix, "HOCA"), all_constructs_of_mode(mmMatrix, "HOCB")))
  mm_items <- unlist(sapply(non_hoc_constructs,
    function(c) construct_indicators(c, mmMatrix), USE.NAMES = FALSE))
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

# warning_struc_meas_model_complete <- function(smMatrix, mmMatrix, data) {
#   construct <- unique(as.vector(smMatrix))
#   constructmm <- unique(as.vector(mmMatrix[, 1]))
#   if(any(construct %in% colnames(data))) {
#     stop("The construct variables cannot share names with the manifest variables.")
#   }
#   manifest <- sort(setdiff(as.vector(mmMatrix[, 1:2]), constructmm))
#
#   if(!all(manifest %in% colnames(data))) {
#     stop("The manifest variables must occur as columns in the data.")
#   }
#   if(!all(construct %in% constructmm)) {
#     stop("The construct variables described in the structural model must occur in the measurement model.")
#   }
# }

# Warning for a dot used in columns of data prior to generating interactions
warning_periods_in_col_names <- function(data) {
  if(TRUE %in% is_interaction(colnames(data))) {
    stop("The names of columns in the data may not contain stars(*)")
  }
}

warnings <- function(mmMatrix,data, smMatrix) {
  warning_single_item_formative(mmMatrix)
  warning_missing_data(data, mmMatrix)
  #warning_periods_in_col_names(data)
}

