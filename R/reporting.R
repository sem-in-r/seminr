#' Functions for reporting the Path Coefficients and R2 of endogenous constructs
#' and for generating a scatterplot matrix of construct scores.
#'
#' \code{report_paths} generates an easy to read table reporting path coefficients
#'   and R2 values for endogenous constructs.\code{plot_scores} generates a
#'   scatterplot matrix of each construct's scores against every other construct's scores.
#'
#' These functions generate an easy to read table reporting path coefficients
#'   and R2 values for endogenous constructs or a scatterplot matrix of construct
#'   scores.
#'
#' @param seminr_model The PLS model estimated by \code{seminr}. The estimated model
#'   returned by the \code{estimate_pls} or \code{bootstrap_model} methods.
#'
#' @param digits A \code{numeric} minimum number of significant digits. If not
#'   specified, default is "2".
#'
#' @param constructs a \code{list} indicating which constructs to report. If not
#'   specified, all constructs are graphed and returned.
#'
#' @usage
#' report_paths(seminr_model, digits=3)
#'
#' plot_scores(seminr_model, constructs=NULL)
#'
#' @examples
#' data(mobi)
#'
#' # seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Value",        multi_items("PERV", 1:2)),
#'   composite("Satisfaction", multi_items("CUSA", 1:3))
#' )
#'
#' #  structural model: note that name of the interactions construct should be
#' #  the names of its two main constructs joined by a '.' in between.
#' mobi_sm <- relationships(
#'   paths(to = "Satisfaction",
#'         from = c("Image", "Expectation", "Value"))
#' )
#'
#' mobi_pls <- estimate_pls(mobi, measurement_model = mobi_mm, structural_model = mobi_sm)
#' report_paths(mobi_pls)
#' plot_scores(mobi_pls)
#'
#' @aliases plot_scores report_paths
#'
#' @export
report_paths <- function(seminr_model, digits=3) {
  endogenous <- unique(seminr_model$smMatrix[,"target"])
  exogenous <- unique(seminr_model$smMatrix[,"source"])
  construct <- seminr_model$constructs

  # create matrix of relevant path coefficients and NAs otherewise
  path_matrix <- matrix(nrow = length(construct), ncol = length(construct), dimnames = list(construct, construct))
  path_matrix[seminr_model$path_coef != 0] <- seminr_model$path_coef[seminr_model$path_coef != 0]

  # add R Squared row
  # Remove BIC for now
  #r_sq <- matrix(nrow = 3, ncol = length(construct), dimnames = list(c("R^2", "AdjR^2", "BIC"), construct))
  r_sq <- matrix(nrow = 2, ncol = length(construct), dimnames = list(c("R^2", "AdjR^2"), construct))
  r_sq[,colnames(seminr_model$rSquared)] <- seminr_model$rSquared
  path_matrix <- rbind(r_sq, path_matrix)

  # round and print
  # Remove BIC for now
  #final_paths <- round(path_matrix[c("R^2","AdjR^2","BIC", exogenous), endogenous, drop=FALSE], digits)
  final_paths <- round(path_matrix[c("R^2","AdjR^2", exogenous), endogenous, drop=FALSE], digits)
  final_paths
}

report_bootstrapped_paths <- function(seminr_model, na.print=".", digits=3) {
  bootstrapresults <- seminr_model$bootstrapMatrix
  nboots <- seminr_model$boots
  bootstraplist <- list()
  j <- ncol(bootstrapresults)/3
  k <- j+1
  l <- (j*2)+1
  for(i in 1:j){
    bootstraplist[[i]] <- bootstrapresults[,c(i,k,l)]
    bootstraplist[[i]] <- cbind(bootstraplist[[i]],matrix((abs(bootstraplist[[i]][,1])/abs(bootstraplist[[i]][,3])),ncol = 1, dimnames = list(c(NULL),c("t value"))))
    bootstraplist[[i]] <- cbind(bootstraplist[[i]], matrix(2*stats::pt(-abs(bootstraplist[[i]][,4]),df = nboots - 1),ncol = 1, dimnames = list(c(NULL),c("Pr(>|t|)"))))
    bootstraplist[[i]][is.nan(bootstraplist[[i]])] <- 0
    #      bootstraplist[[i]] <- cbind(bootstraplist[[i]], bootstraplist[[i]][bootstraplist[[i]][,5] == 0,5] = "")
    k <- k+1
    l <- l+1
  }

  for(i in 1:length(bootstraplist)) { bootstraplist[[i]] <- round(bootstraplist[[i]], digits) }

  # print final_boot
  for(i in 1:length(bootstraplist)) { print(bootstraplist[[i]], na.print = na.print) }

  class(bootstraplist) <- "report_bootstrapped_paths"
  bootstraplist
}

#' @export
plot_scores <- function(seminr_model, constructs=NULL) {
#  if (class(seminr_model)[1] == 'plsModel') seminr_model <- seminr_model
  if (missing(constructs)) constructs <- seminr_model$constructs

  graphics::plot(as.data.frame(seminr_model$construct_scores[, constructs]), pch = 16,
       col = grDevices::rgb(0.5, 0.5, 0.5, alpha = 0.6))
}
