#' Functions for reporting the Path Coefficients and R2 of endogenous constructs
#' and for generating a scatterplot matrix of factor scores.
#'
#' \code{print_paths} generates an easy to read table reporting path coefficients
#'   and R2 values for endogenous constructs.\code{plot_scores} generates a
#'   scatterplot matrix of each factor's scores against every other factor's scores.
#'
#' These functions generate an easy to read table reporting path coefficients
#'   and R2 values for endogenous constructs or a scatterplot matrix of factor
#'   scores.
#'
#' @param seminr_model The PLS model estimated by simplePLS \code{seminr}. The estimated model
#'   returned by the \code{estimate_pls} or \code{bootstrap_model} methods.
#'
#' @param na.print A \code{character} substituting values not to be printed.
#'   If not specified, default = "."
#'
#' @param digits A \code{numeric} minimum number of significant digits. If not
#'   specified, default is "2".
#'
#' @param factors a \code{list} indicating which factors to report. If not
#'   specified, all factors are graphed and returned.
#'
#' @usage
#' print_paths(model, na.print=".", digits=2)
#'
#' plot_scores(fitted_model, factors=NULL)
#'
#' @examples
#' mobi <- mobi
#' mobi_pls <- estimate_pls(data = mobi,
#'                            measurement_model = mobi_mm,
#'                            structural_model = mobi_sm)
#' print_paths(mobi_pls)
#' plot_scores(mobi_pls)
#'
#' # Estimate model with bootstrapped significance
#' mobi_pls <- bootstrap_model(data = mobi,
#'                            measurement_model = mobi_mm,
#'                            structural_model = mobi_sm,
#'                            nboot = 200)
#'
#' print_paths(mobi_pls)
#' plot_scores(mobi_pls)
#'
#' @aliases plot_scores print_paths
#'
#' @export
print_paths <- function(seminr_model, na.print=".", digits=2) {

  # bootstrap results
  if (!is.null(seminr_model$bootstrapMatrix)) {
    bootstrapresults <- seminr_model$bootstrapMatrix
    nboots <- seminr_model$boots
    bootstraplist <- list()
    j <- ncol(bootstrapresults)/3
    k <- j+1
    l <- (j*2)+1
    for(i in 1:j){
      bootstraplist[[i]] <- bootstrapresults[,c(i,k,l)]
      bootstraplist[[i]] <- cbind(bootstraplist[[i]],matrix((abs(bootstraplist[[i]][,1])/abs(bootstraplist[[i]][,3])),ncol = 1, dimnames = list(c(NULL),c("t value"))))
      bootstraplist[[i]] <- cbind(bootstraplist[[i]], matrix(2*pt(-abs(bootstraplist[[i]][,4]),df = nboots - 1),ncol = 1, dimnames = list(c(NULL),c("Pr(>|t|)"))))
      bootstraplist[[i]][is.nan(bootstraplist[[i]])] <- 0
#      bootstraplist[[i]] <- cbind(bootstraplist[[i]], bootstraplist[[i]][bootstraplist[[i]][,5] == 0,5] = "")
      k <- k+1
      l <- l+1
    }

    for(i in 1:length(bootstraplist)) { bootstraplist[[i]] <- round(bootstraplist[[i]], digits) }

    # print final_boot
    for(i in 1:length(bootstraplist)) { print(bootstraplist[[i]], na.print = na.print) }

  }else {

   endogenous <- unique(seminr_model$smMatrix[,"target"])
   exogenous <- unique(seminr_model$smMatrix[,"source"])
   latent <- seminr_model$ltVariables

   # create matrix of relevant path coefficients and NAs otherewise
   path_matrix <- matrix(nrow = length(latent), ncol = length(latent), dimnames = list(latent, latent))
   path_matrix[seminr_model$path_coef != 0] <- seminr_model$path_coef[seminr_model$path_coef != 0]

   # add R Squared row
   r_sq <- matrix(nrow = 2, ncol = length(latent), dimnames = list(c("R^2","AdjR^2"), latent))
   r_sq[,colnames(seminr_model$rSquared)] <- seminr_model$rSquared
   path_matrix <- rbind(r_sq, path_matrix)

   # round and print
   final_paths <- round(path_matrix[c("R^2","AdjR^2", exogenous), endogenous, drop=FALSE], digits)
   print(final_paths, na.print = na.print)
  }
}
#' @export
plot_scores <- function(seminr_model, factors=NULL) {
#  if (class(seminr_model)[1] == 'plsModel') seminr_model <- seminr_model
  if (missing(factors)) factors <- seminr_model$ltVariables

  plot(as.data.frame(seminr_model$fscores[, factors]), pch = 16,
       col = rgb(0.5, 0.5, 0.5, alpha = 0.6))
}
