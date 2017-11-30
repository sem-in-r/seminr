# Warning to check for formative only constructs
warning_only_causal_construct <- function(mmMatrix) {
  latents <- unique(mmMatrix[,1])

  for(latent in latents) {
    if(length(items_per_mode(latent,"B",mmMatrix)) == nrow(mmMatrix_per_latent(latent,mmMatrix))) {
      warning(c(latent," is purely defined as a causal formative construct.\n"))
    }
  }
}

warning_single_item_formative <- function(mmMatrix) {
  ltVariables <- unique(mmMatrix[,1])
  for(latent in ltVariables) {
    if(nrow(mmMatrix_per_latent(latent,mmMatrix)) == 1 && mmMatrix_per_latent(latent,mmMatrix)[,3] == "B") {
      stop("You cannot define a single item latent as formative")
    }
  }
}

warning_missing_data <- function(data, mmMatrix) {
  data <- data[, mmMatrix[which(!grepl("\\.", mmMatrix[,2])),2]]
  N <- nrow(data)
  missing_values <- which(complete.cases(data)==FALSE)
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
  latent <- unique(as.vector(smMatrix))
  latentmm <- unique(as.vector(mmMatrix[,1]))
  if(any(latent %in% colnames(data))) {
    stop("The latent variables cannot share names with the manifest variables.")
  }
  manifest <- sort(setdiff(as.vector(mmMatrix[,1:2]), latent))

  if(!all(manifest %in% colnames(data))) {
    stop("The manifest variables must occur as columns in the data.")
  }
  if(!all(latent %in% latentmm)) {
    stop("The latent variables described in the structural model must occur in the measurement model.")
  }
}

warnings <- function(mmMatrix,data, smMatrix) {
  warning_only_causal_construct(mmMatrix)
  warning_single_item_formative(mmMatrix)
  warning_missing_data(data, mmMatrix)
}

