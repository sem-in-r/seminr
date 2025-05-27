#' @export
plot_scores <- function(seminr_model, constructs=NULL) {
  #  if (class(seminr_model)[1] == 'plsModel') seminr_model <- seminr_model
  if (missing(constructs)) constructs <- seminr_model$constructs

  graphics::plot(as.data.frame(seminr_model$construct_scores[, constructs]), pch = 16,
                 col = grDevices::rgb(0.5, 0.5, 0.5, alpha = 0.6))
}

#' Function for plotting the measurement model reliability metrics of a PLS model
#'
#' \code{plot.reliability_table} generates an easy to read visualization of the rhoA, Cronbachs
#'   alpha, and Composite Reliability for all constructs. The plot visualizes the metrics
#'   in such a way as to draw meaning from not only the absolute values, but their relative
#'   values too.
#'
#' @param x A \code{reliability_table} object from a SEMinR PLS model. This can be accessed
#'   as the \code{reliability} element of the PLS model summary object.
#'
#' @param ... All other arguments inherited from \code{plot}.
#'
#' @examples
#'data(mobi)
#'
#'# seminr syntax for creating measurement model
#'mobi_mm <- constructs(
#'  composite("Image",        multi_items("IMAG", 1:5)),
#'  composite("Expectation",  multi_items("CUEX", 1:3)),
#'  composite("Value",        multi_items("PERV", 1:2)),
#'  composite("Satisfaction", multi_items("CUSA", 1:3))
#')
#'
#'#  structural model: note that name of the interactions construct should be
#'#  the names of its two main constructs joined by a '*' in between.
#'mobi_sm <- relationships(
#'  paths(to = "Satisfaction",
#'        from = c("Image", "Expectation", "Value"))
#')
#'
#'mobi_pls <- estimate_pls(mobi, measurement_model = mobi_mm, structural_model = mobi_sm)
#'plot(summary(mobi_pls)$reliability)
#'
#' @export
plot.reliability_table <- function(x, ...) {
  stopifnot(inherits(x, "reliability_table"))

  metrics <- cbind(matrix(1:nrow(x),ncol = 1), x)
  lower_lim <- ifelse(min(as.numeric(metrics[,-1]) - 0.2) >= 0.6, 0.6, min(as.numeric(metrics[,-1]) - 0.2))
  graphics::plot(metrics[,1:2], xlim=c(0.7, nrow(metrics[,-1])+0.2), ylim=c(lower_lim, max(as.numeric(metrics[,-1]))),
       frame.plot = FALSE, xaxt='n', ylab='', xlab = '', pch='')

  # Grid
  graphics::grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

  # Add a legend
  graphics::legend("bottom",
         legend = c("alpha", "RhoA", "RhoC"),
         col = c("black", "black", "black"),
         pch = c(19, 15,17),
         bty = "n",
         pt.cex = 1,
         cex = 1,
         text.col = "black",
         horiz = TRUE,
         inset = c(0.1, 0.1))

  # rhoA line and shape
  graphics::segments(unlist(metrics[,1])-0.2, unlist(metrics[, "rhoA"]), unlist(metrics[,1])+0.2, unlist(metrics[, "rhoA"]))
  graphics::points(unlist(metrics[,1]), unlist(metrics[, "rhoA"]), pch=15)

  # alpha line and shape
  graphics::segments(unlist(metrics[,1])-0.1, unlist(metrics[, "rhoA"]), unlist(metrics[,1])-0.1, unlist(metrics[, "alpha"]))
  graphics::points(unlist(metrics[,1])-0.1, unlist(metrics[, "alpha"]), pch=19)

  # rhoC line and shape
  graphics::segments(unlist(metrics[,1])+0.1, unlist(metrics[, "rhoA"]), unlist(metrics[,1])+0.1, unlist(metrics[, "rhoC"]))
  graphics::points(unlist(metrics[,1])+0.1, unlist(metrics[, "rhoC"]), pch=17)

  # threshhold line
  graphics::abline(h = 0.7, lty = 2, col = "blue")

  # rotated axis labels: https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
  graphics::axis(side = 1, labels = FALSE)
  graphics::text(x = 1:nrow(metrics), y = graphics::par("usr")[3] - 0.03,
       labels = rownames(metrics), xpd = NA, srt = 35, adj = 0.965)

  invisible(x)
}

#' Function for plotting interaction plot for moderated PLS or CBSEM model
#'
#' \code{plot_interaction} generates an interaction plot for the effect of an antecedent
#'   on an outcome given a mediator variable.
#'
#' @param moderated_model SEMinR model that contains an interaction.
#'
#' @param intxn Name (character) of the interaction term in the structural model. Must look like a product of independent variabel and moderator (e.g., "ABC*XYZ")
#'
#' @param dv Name (character) of the dependant consutruct affected by the moderator.
#'
#' @param legend Location (character) of the legend on the plot; must be a combination of bottom|top and left|right (e.g., "bottomright").
#'
#' @usage
#' plot_interaction(moderated_model, intxn, dv, legend)
#'
#' @examples
#' data(mobi)
#'
#' # seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#' composite("Image",        multi_items("IMAG", 1:5)),
#' composite("Expectation",  multi_items("CUEX", 1:3)),
#' composite("Value",        multi_items("PERV", 1:2)),
#' composite("Satisfaction", multi_items("CUSA", 1:3)),
#' interaction_term(iv = "Image", moderator = c("Expectation"), method = orthogonal))
#'
#' # Structural model
#' #  note: interactions should be the names of its main constructs joined by a '*' in between.
#' mobi_sm <- relationships(
#'   paths(to = "Satisfaction",
#'         from = c("Image", "Expectation", "Value",
#'                  "Image*Expectation")))
#'
#' # Load data, assemble model, and estimate
#' mobi_pls <- estimate_pls(data = mobi,
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm)
#'
#' plot_interaction(mobi_pls, "Image*Expectation", "Satisfaction", "bottomright")
#'
#' @export
plot_interaction <- function(moderated_model, intxn, dv, legend = "bottomright") {
  intxn_terms <- strsplit(intxn, "*", fixed=TRUE)[[1]]
  iv = intxn_terms[1]
  mod = intxn_terms[2]

  slope_analysis(moderated_model, dv=dv, iv=iv, moderator=mod, leg_place=legend)
}

#' Function for plotting a slope analysis for an interaction in a PLS model
#'
#' \code{slope_analysis} generates an interaction plot for the effect of an antecedent
#'   on an outcome given a mediator variable.
#'
#' @param moderated_model A SEMinR model that contains an interaction.
#'
#' @param dv The name of the dependant consutruct affected by the moderator (interaction term).
#'
#' @param moderator The name of the moderator construct.
#'
#' @param iv The name of the independant construct affected by the moderator.
#'
#' @param leg_place The location of the legend, in order to make sure the legend does not
#'   obscure the plot lines.
#'
#' @usage
#' slope_analysis(moderated_model, dv, moderator, iv,  leg_place)
#'
#' @examples
#' data(mobi)
#'
#' # seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#' composite("Image",        multi_items("IMAG", 1:5)),
#' composite("Expectation",  multi_items("CUEX", 1:3)),
#' composite("Value",        multi_items("PERV", 1:2)),
#' composite("Satisfaction", multi_items("CUSA", 1:3)),
#' interaction_term(iv = "Image", moderator = c("Expectation"), method = orthogonal))
#'
#' # Structural model
#' #  note: interactions should be the names of its main constructs joined by a '*' in between.
#' mobi_sm <- relationships(
#'   paths(to = "Satisfaction",
#'         from = c("Image", "Expectation", "Value",
#'                  "Image*Expectation")))
#'
#' # Load data, assemble model, and estimate
#' mobi_pls <- estimate_pls(data = mobi,
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm)
#'
#' slope_analysis(mobi_pls, "Satisfaction", "Expectation", "Image", "bottomright")
#'
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

  graphics::plot(c(-1,0,1), res[c(1,4,7)], type="n", xlab = iv, ylab = dv,
       xlim = c(-1,1), ylim = c(min(res),max(res)))
  graphics::lines(c(-1,0,1), res[c(1,4,7)], lty = 2)
  graphics::lines(c(-1,0,1), res[c(2,5,8)], lty = 1)
  graphics::lines(c(-1,0,1), res[c(3,6,9)], lty = 3)
  graphics::grid()
  graphics::legend(
    leg_place,
    paste(moderator, c("at -1SD", "at Mean", "at +1SD")),
    lty=c(2,1,3), horiz=FALSE, bty="n", cex = 0.8
  )
}
