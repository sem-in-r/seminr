# Warning to check for formative only constructs
warning_only_causal_construct <- function(mmMatrix) {
  latents <- unique(mmMatrix[,1])

  for(latent in latents) {
    if(length(items_per_mode(latent,"F",mmMatrix)) == nrow(mmMatrix_per_latent(latent,mmMatrix))) {
      warning(c(latent," is purely defined as a causal formative construct.\n"))
    }
  }
}

warning_single_item_formative <- function(mmMatrix) {
  ltVariables <- unique(mmMatrix[,1])
  for(latent in ltVariables) {
    if(nrow(mmMatrix_per_latent(latent,mmMatrix)) == 1 && mmMatrix_per_latent(latent,mmMatrix)[,3] == "F") {
      stop("You cannot define a single item latent as formative")
    }
  }
}

warnings <- function(mmMatrix) {
  warning_only_causal_construct(mmMatrix)
  warning_single_item_formative(mmMatrix)
}
