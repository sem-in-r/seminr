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
  data <- data[, mmMatrix[,2]]
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



warnings <- function(mmMatrix,data) {
  warning_only_causal_construct(mmMatrix)
  warning_single_item_formative(mmMatrix)
  warning_missing_data(data, mmMatrix)
}

