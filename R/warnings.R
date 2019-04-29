warning_single_item_formative <- function(mmMatrix) {
  constructs <- unique(mmMatrix[,1])
  for(construct in constructs) {
    if(nrow(mmMatrix_per_construct(construct,mmMatrix)) == 1 && mmMatrix_per_construct(construct,mmMatrix)[,3] == "B") {
      stop("You cannot define a single item construct as mode B")
    }
  }
}

warning_missing_data <- function(data, mmMatrix) {
  data <- data[, mmMatrix[which(!grepl("\\*", mmMatrix[,2])),2]]
  N <- nrow(data)
  missing_values <- which(stats::complete.cases(data)==FALSE)
  if(length(missing_values)==0){
    cat("All", N ,"observations are valid.\n")
  }
  else {
    cat("Data rows", paste(missing_values, collapse=", "),
        " contain missing values and will be omitted.\n",
        "Total number of complete cases:", N-length(missing_values), "\n")
  }
}

warning_struc_meas_model_complete <- function(smMatrix, mmMatrix, data) {
  construct <- unique(as.vector(smMatrix))
  constructmm <- unique(as.vector(mmMatrix[,1]))
  if(any(construct %in% colnames(data))) {
    stop("The construct variables cannot share names with the manifest variables.")
  }
  manifest <- sort(setdiff(as.vector(mmMatrix[,1:2]), construct))

  if(!all(manifest %in% colnames(data))) {
    stop("The manifest variables must occur as columns in the data.")
  }
  if(!all(construct %in% constructmm)) {
    stop("The construct variables described in the structural model must occur in the measurement model.")
  }
}

# Warning for a dot used in columns of data prior to generating interactions
warning_periods_in_col_names <- function(data) {
  if(TRUE %in% grepl("\\*", colnames(data))) {
    stop("The names of columns in the data may not contain stars(*)")
  }
}

warnings <- function(mmMatrix,data, smMatrix) {
  warning_single_item_formative(mmMatrix)
  warning_missing_data(data, mmMatrix)
  warning_periods_in_col_names(data)
}

