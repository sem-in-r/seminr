#' @export
warning_only_formative_construct <- function(mmMatrix) {
  latents <- unique(mmMatrix[,1])

  for(latent in latents) {
    if(length(items_per_mode(latent,"F",mmMatrix)) == nrow(mmMatrix[mmMatrix[,"latent"] == latent,])) {
      cat(c("Warning:",latent,"is purely formatively defined\n"))
    }
  }
}
