# Warning to check for formative only constructs
warning_only_formative_construct <- function(mmMatrix) {
  latents <- unique(mmMatrix[,1])

  for(latent in latents) {
    if(length(items_per_mode(latent,"F",mmMatrix)) == nrow(mmMatrix_per_latent(latent,mmMatrix))) {
      cat(c("*** Warning:",latent,"is purely formatively defined. ***\n"))
    }
  }
}
