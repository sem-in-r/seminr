#' @export
plot_scores <- function(seminr_model, constructs=NULL) {
  #  if (class(seminr_model)[1] == 'plsModel') seminr_model <- seminr_model
  if (missing(constructs)) constructs <- seminr_model$constructs

  graphics::plot(as.data.frame(seminr_model$construct_scores[, constructs]), pch = 16,
                 col = grDevices::rgb(0.5, 0.5, 0.5, alpha = 0.6))
}


#' Plot Reliability Tablecan be used to generate simple and effective bar plots for reliability stats.
#' @export
plot.reliability_table <- function(object) {
  stopifnot(inherits(object, "reliability_table"))
  limit <- c(0.7,0.7,0.5,0.7)
  for (i in 1:ncol(object)) {
    vec <- object[,i]
    graphics::barplot(vec,
            col = ifelse(vec > limit[i], "green", "red"),
            main = colnames(object)[i],
            panel.first = graphics::grid())
    graphics::abline(h = limit[i], lty = 2, col = "blue")
  }
  invisible(object)
}
