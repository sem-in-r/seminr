#' @export
plot_scores <- function(seminr_model, constructs=NULL) {
  #  if (class(seminr_model)[1] == 'plsModel') seminr_model <- seminr_model
  if (missing(constructs)) constructs <- seminr_model$constructs

  graphics::plot(as.data.frame(seminr_model$construct_scores[, constructs]), pch = 16,
                 col = grDevices::rgb(0.5, 0.5, 0.5, alpha = 0.6))
}
