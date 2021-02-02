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
#' #  the names of its two main constructs joined by a '*' in between.
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
  final_paths <- path_matrix[c("R^2","AdjR^2", exogenous), endogenous, drop=FALSE]
  convert_to_table_output(final_paths)
}

# report_bootstrapped_paths <- function(boot_seminr_model, na.print=".", digits=3) {
#   bootstrapresults <- seminr_model$
#   nboots <- boot_seminr_model$boots
#   bootstraplist <- list()
#   j <- ncol(bootstrapresults)/3
#   k <- j+1
#   l <- (j*2)+1
#   for(i in 1:j){
#     bootstraplist[[i]] <- bootstrapresults[,c(i,k,l)]
#     bootstraplist[[i]] <- cbind(bootstraplist[[i]],matrix((abs(bootstraplist[[i]][,1])/abs(bootstraplist[[i]][,3])),ncol = 1, dimnames = list(c(NULL),c("t value"))))
#     bootstraplist[[i]] <- cbind(bootstraplist[[i]], matrix(2*stats::pt(-abs(bootstraplist[[i]][,4]),df = nboots - 1),ncol = 1, dimnames = list(c(NULL),c("Pr(>|t|)"))))
#     bootstraplist[[i]][is.nan(bootstraplist[[i]])] <- 0
#     #      bootstraplist[[i]] <- cbind(bootstraplist[[i]], bootstraplist[[i]][bootstraplist[[i]][,5] == 0,5] = "")
#     k <- k+1
#     l <- l+1
#   }
#
#   for(i in 1:length(bootstraplist)) { bootstraplist[[i]] <- round(bootstraplist[[i]], digits) }
#
#   # print final_boot
#   for(i in 1:length(bootstraplist)) { print(bootstraplist[[i]], na.print = na.print) }
#
#   class(bootstraplist) <- "report_bootstrapped_paths"
#   bootstraplist
# }

#' seminr specific effect significance function
#'
#' The \code{seminr} package provides a natural syntax for researchers to describe PLS
#' structural equation models.
#' \code{specific_effect_significance} provides the verb for calculating the bootstrap mean, standard deviation, T value,
#'  and confidence intervals for direct or mediated path in a bootstrapped SEMinR model.
#'
#' @param boot_seminr_model A bootstrapped model returned by the \code{bootstrap_model} function.
#'
#' @param from A parameter specifying the antecedent composite for the path.
#'
#' @param to A parameter specifying the outcome composite for the path.
#'
#' @param through A parameter to specify a vector of mediators for the path. Default is NULL.
#'
#' @param alpha A parameter for specifying the alpha for the confidence interval. Default is 0.05.
#'
#' @usage
#' specific_effect_significance(boot_seminr_model, from, to, through, alpha)
#'
#' @seealso \code{\link{bootstrap_model}}
#'
#' @references Zhao, X., Lynch Jr, J. G., & Chen, Q. (2010). Reconsidering Baron and Kenny: Myths and truths
#' about mediation analysis. Journal of consumer research, 37(2), 197-206.
#'
#' @examples
#' mobi_mm <- constructs(
#' composite("Image",        multi_items("IMAG", 1:5)),
#' composite("Expectation",  multi_items("CUEX", 1:3)),
#' composite("Quality",      multi_items("PERQ", 1:7)),
#' composite("Value",        multi_items("PERV", 1:2)),
#' composite("Satisfaction", multi_items("CUSA", 1:3)),
#' composite("Complaints",   single_item("CUSCO")),
#' composite("Loyalty",      multi_items("CUSL", 1:3))
#' )
#'
#' # Creating structural model
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' # Estimating the model
#' mobi_pls <- estimate_pls(data = mobi,
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm)
#'
#' # Load data, assemble model, and bootstrap
#' boot_seminr_model <- bootstrap_model(seminr_model = mobi_pls,
#'                                      nboot = 50, cores = 2, seed = NULL)
#'
#' specific_effect_significance(boot_seminr_model = boot_seminr_model,
#'                              from = "Image",
#'                              through = c("Expectation", "Satisfaction","Complaints"),
#'                              to = "Loyalty",
#'                              alpha = 0.05)
#' @export
specific_effect_significance <- function(boot_seminr_model, from, to, through = NULL, alpha = 0.05) {
  path_array <- boot_seminr_model$boot_paths
  orig_matrix <- boot_seminr_model$path_coef
  if (is.null(through)) {
    coefficient <- path_array[from, to,]
    orig_coefficient <- orig_matrix[from,to]
  } else {
    if (length(through) > 4) {
      message("Currently only serial mediation with 4 mediating variables is allowed ")
      return(NULL)
    } else {
      coefficient <- switch(length(through),
                            path_array[from, through[1],] * path_array[through[1], to,],
                            path_array[from, through[1],] * path_array[through[1], through[2],] * path_array[through[2], to,],
                            path_array[from, through[1],] * path_array[through[1], through[2],] * path_array[through[2], through[3],] * path_array[through[3], to,],
                            path_array[from, through[1],] * path_array[through[1], through[2],] * path_array[through[2], through[3],] * path_array[through[3], through[4],] * path_array[through[4], to, ])
      orig_coefficient <- switch(length(through),
                                 orig_matrix[from, through[1]] * orig_matrix[through[1], to],
                                 orig_matrix[from, through[1]] * orig_matrix[through[1], through[2]] * orig_matrix[through[2], to],
                                 orig_matrix[from, through[1]] * orig_matrix[through[1], through[2]] * orig_matrix[through[2], through[3]] * orig_matrix[through[3], to],
                                 orig_matrix[from, through[1]] * orig_matrix[through[1], through[2]] * orig_matrix[through[2], through[3]] * orig_matrix[through[3], through[4]] * orig_matrix[through[4], to ])
    }
  }
  sd_estimate <- sd(coefficient)
  mean_estimate <- mean(coefficient)
  quantiles <- stats::quantile(coefficient, probs = c(alpha/2,1-(alpha/2)))
  return_vec <- c(orig_coefficient, mean_estimate, sd_estimate, orig_coefficient/sd_estimate,quantiles )
  names(return_vec) <- c( "Original Est.", "Bootstrap Mean", "Bootstrap SD", "T Stat.",paste(alpha, "% CI", sep = ""),paste((1-alpha), "% CI", sep = ""))
  return(return_vec)
}

#' seminr total indirect confidence intervals function
#'
#' \code{total_indirect_ci} provides the verb for calculating the total indirect confidence intervals of a
#' direct or mediated path in a bootstrapped SEMinR model.
#'
#' @param boot_seminr_model A bootstrapped model returned by the \code{bootstrap_model} function.
#'
#' @param from A parameter specifying the antecedent composite for the path.
#'
#' @param to A parameter specifying the outcome composite for the path.
#'
#' @param alpha A parameter for specifying the alpha for the confidence interval. Default is 0.05.
#'
#' @usage
#' total_indirect_ci(boot_seminr_model, from, to, alpha)
#'
#' @seealso \code{\link{bootstrap_model}}
#'
#' @references Zhao, X., Lynch Jr, J. G., & Chen, Q. (2010). Reconsidering Baron and Kenny: Myths and truths
#' about mediation analysis. Journal of consumer research, 37(2), 197-206.
#'
#' @examples
#' mobi_mm <- constructs(
#' composite("Image",        multi_items("IMAG", 1:5)),
#' composite("Expectation",  multi_items("CUEX", 1:3)),
#' composite("Quality",      multi_items("PERQ", 1:7)),
#' composite("Value",        multi_items("PERV", 1:2)),
#' composite("Satisfaction", multi_items("CUSA", 1:3)),
#' composite("Complaints",   single_item("CUSCO")),
#' composite("Loyalty",      multi_items("CUSL", 1:3))
#' )
#'
#' # Creating structural model
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' # Estimating the model
#' mobi_pls <- estimate_pls(data = mobi,
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm)
#'
#' # Load data, assemble model, and bootstrap
#' boot_seminr_model <- bootstrap_model(seminr_model = mobi_pls,
#'                                      nboot = 50, cores = 2, seed = NULL)
#'
#' total_indirect_ci(boot_seminr_model = boot_seminr_model,
#'                   from = "Image",
#'                   to = "Loyalty",
#'                   alpha = 0.05)
#' @export
total_indirect_ci <- function(boot_seminr_model, from, to, alpha = 0.05) {
  path_array <- boot_seminr_model$boot_paths
  total_array <- boot_seminr_model$boot_total_paths
  coefficient <- total_array[from, to,] - path_array[from, to,]
  quantiles <- stats::quantile(coefficient, probs = c(alpha/2,1-(alpha/2)))
  return(quantiles)
}

parse_boot_array <- function(original_matrix, boot_array, alpha = 0.05) {
  Path <- c()
  original <- c()
  boot_mean <- c()
  boot_SD <- c()
  t_stat <- c()
  lower <- c()
  upper <- c()
  alpha_text <- alpha/2*100
  original_matrix[is.na(original_matrix)] <- 0
  for (i in 1:nrow(original_matrix)) {
    for (j in 1:ncol(original_matrix)) {
      if (original_matrix[i,j]!=0 ) {
        Path <- append(Path, paste(rownames(original_matrix)[i], " -> ", colnames(original_matrix)[j]))
        original <- append(original, original_matrix[i,j])
        boot_mean <- append(boot_mean, mean(boot_array[i,j,]))
        boot_SD <- append(boot_SD, stats::sd(boot_array[i,j,]))
        if (original_matrix[i,j]/ stats::sd(boot_array[i,j,]) > 999999999) {
          t_stat <- append(t_stat, NA)
        } else {
          t_stat <- append(t_stat,  original_matrix[i,j]/ stats::sd(boot_array[i,j,]))
        }
        lower <- append(lower, (conf_int(boot_array, from = rownames(original_matrix)[i], to = colnames(original_matrix)[j], alpha = alpha))[[1]])
        upper <- append(upper, (conf_int(boot_array, from = rownames(original_matrix)[i], to = colnames(original_matrix)[j], alpha = alpha))[[2]])
      }
    }
  }
  return_matrix <- cbind(original, boot_mean, boot_SD, t_stat, lower, upper)
  colnames(return_matrix) <- c( "Original Est.", "Bootstrap Mean", "Bootstrap SD", "T Stat.",paste(alpha_text, "% CI", sep = ""),paste((100-alpha_text), "% CI", sep = ""))
  rownames(return_matrix) <- Path
  convert_to_table_output(return_matrix)
}
