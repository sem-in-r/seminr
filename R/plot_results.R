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

  metrics <- cbind(1:nrow(x), x)
  plot(metrics[,1:2], xlim=c(0.7, nrow(metrics[,-1])+0.3), ylim=c(min(metrics[,-1] - 0.1), max(metrics[,-1])),
       frame.plot = FALSE, xaxt='n', ylab='', xlab = '', pch='')

  # Grid
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

  # Add a legend
  legend("bottom",
         legend = c("alpha", "RhoA", "RhoC"),
         col = c("black", "black", "black"),
         pch = c(19, 15,17),
         bty = "n",
         pt.cex = 1,
         cex = 1,
         text.col = "black",
         horiz = T ,
         inset = c(0.1, 0.1))

  # rhoA line and shape
  graphics::segments(metrics[,1]-0.2, metrics[, "rhoA"], metrics[,1]+0.2, metrics[, "rhoA"])
  graphics::points(metrics[,1], metrics[, "rhoA"], pch=15)

  # alpha line and shape
  graphics::segments(metrics[,1]-0.1, metrics[, "rhoA"], metrics[,1]-0.1, metrics[, "alpha"])
  graphics::points(metrics[,1]-0.1, metrics[, "alpha"], pch=19)

  # rhoC line and shape
  graphics::segments(metrics[,1]+0.1, metrics[, "rhoA"], metrics[,1]+0.1, metrics[, "rhoC"])
  graphics::points(metrics[,1]+0.1, metrics[, "rhoC"], pch=17)

  # threshhold line
  graphics::abline(h = 0.708, lty = 2, col = "blue")

  # rotated axis labels: https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
  axis(side = 1, labels = FALSE)
  text(x = 1:nrow(metrics), y = par("usr")[3] - 0.03,
       labels = rownames(metrics), xpd = NA, srt = 35, adj = 0.965)

  invisible(x)
}

#' @export
slope_analysis <- function(moderated_model, dv, moderator, iv, leg_place = "bottomright") {
  mat <- matrix(c(-1, 1, -1,
                  -1, 0,  0,
                  -1,-1,  1,
                  0, 0, -1,
                  0, 0,  0,
                  0, 0,  1,
                  1,-1, -1,
                  1, 0,  0,
                  1, 1,  1), nrow = 9, ncol = 3, byrow = TRUE)
  res <- mat %*% moderated_model$path_coef[c(iv,paste(iv,"*",moderator, sep = ""),moderator),dv]

  plot(c(-1,0,1), res[c(1,4,7)], type="n", xlab = iv, ylab = dv,
       xlim = c(-1,1), ylim = c(min(res),max(res)))
  lines(c(-1,0,1), res[c(1,4,7)], lty = 2)
  lines(c(-1,0,1), res[c(2,5,8)], lty = 1)
  lines(c(-1,0,1), res[c(3,6,9)], lty = 3)
  grid()
  legend(leg_place, c("Mod at -1SD", "Mod at Mean", "Mod at 1SD"), lty=c(2,1,3),
         horiz=FALSE, bty="n", cex = 0.8
  )
}




