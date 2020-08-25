#' @export
plot_scores <- function(seminr_model, constructs=NULL) {
  #  if (class(seminr_model)[1] == 'plsModel') seminr_model <- seminr_model
  if (missing(constructs)) constructs <- seminr_model$constructs

  graphics::plot(as.data.frame(seminr_model$construct_scores[, constructs]), pch = 16,
                 col = grDevices::rgb(0.5, 0.5, 0.5, alpha = 0.6))
}

#' @export
plot.reliability_table <- function(x, ...) {
  stopifnot(inherits(x, "reliability_table"))
  graphics::barplot(t(x[,c(1,2,4)]),
                    # col=colors()[c(23,89,12)] ,
                    col = ifelse(t(x[,c(1,2,4)]) > 0.7, c("palegreen4"), "orangered3"),
                    border="white",
                    font.axis=1,
                    beside=T,
                    xlab="Reliability",
                    font.lab=2,
                    names.arg =rep(rownames(t(x[,c(1,2,4)])),ncol(t(x[,c(1,2,4)]))),
                    las = 2)
  graphics::abline(h = 0.708, lty = 2, col = "blue")
  invisible(x)
}
