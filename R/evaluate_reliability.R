#' seminr rho_A Function
#'
#' The \code{rho_A} function calculates the rho_A reliability indices for each construct. For
#' formative constructs, the index is set to 1.
#'
#' @param seminr_model A \code{seminr_model} containing the estimated seminr model.
#'
#' @usage
#' rho_A(seminr_model)
#'
#' @seealso \code{\link{relationships}} \code{\link{constructs}} \code{\link{paths}} \code{\link{interaction_term}}
#'          \code{\link{bootstrap_model}}
#'
#' @references Dijkstra, T. K., & Henseler, J. (2015). Consistent partial least squares path modeling. MIS quarterly, 39(2).
#'
#' @examples
#' #seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'              reflective("Image",        multi_items("IMAG", 1:5)),
#'              reflective("Expectation",  multi_items("CUEX", 1:3)),
#'              reflective("Quality",      multi_items("PERQ", 1:7)),
#'              reflective("Value",        multi_items("PERV", 1:2)),
#'              reflective("Satisfaction", multi_items("CUSA", 1:3)),
#'              reflective("Complaints",   single_item("CUSCO")),
#'              reflective("Loyalty",      multi_items("CUSL", 1:3))
#'            )
#' #seminr syntax for creating structural model
#' mobi_sm <- relationships(
#'   paths(from = "Image",        to = c("Expectation", "Satisfaction", "Loyalty")),
#'   paths(from = "Expectation",  to = c("Quality", "Value", "Satisfaction")),
#'   paths(from = "Quality",      to = c("Value", "Satisfaction")),
#'   paths(from = "Value",        to = c("Satisfaction")),
#'   paths(from = "Satisfaction", to = c("Complaints", "Loyalty")),
#'   paths(from = "Complaints",   to = "Loyalty")
#' )
#'
#' mobi_pls <- estimate_pls(data = mobi,
#'                            measurement_model = mobi_mm,
#'                            structural_model = mobi_sm)
#'
#' rho_A(mobi_pls)
#' @export
# rho_A as per Dijkstra, T. K., & Henseler, J. (2015). Consistent Partial Least Squares Path Modeling, 39(X).
rho_A <- function(seminr_model) {
  # get construct variable scores and weights for each construct
  constructscores <- seminr_model$construct_scores
  weights <- seminr_model$outer_weights
  # get the mmMatrix and smMatrix
  mmMatrix <- seminr_model$mmMatrix
  smMatrix <- seminr_model$smMatrix
  obsData <- seminr_model$data
  # Create rho_A holder matrix
  rho <- matrix(, nrow = ncol(constructscores), ncol = 1, dimnames = list(colnames(constructscores), c("rhoA")))

  for (i in rownames(rho))  {
    #If the measurement model is Formative assign rhoA = 1
    if(mmMatrix[mmMatrix[, "construct"]==i, "type"][1] %in% c("B", "HOCB")){ #| mmMatrix[mmMatrix[, "construct"]==i, "type"][1]=="A"){
      rho[i, 1] <- 1
    }
    #If the measurement model is Reflective Calculate RhoA
    if(mmMatrix[mmMatrix[, "construct"]==i, "type"][1] %in% c("C", "A", "HOCA")) {#| mmMatrix[mmMatrix[, "construct"]==i, "type"][1]=="A"|){
      #if the construct is a single item rhoA = 1
      if(nrow(mmMatrix_per_construct(i, mmMatrix)) == 1 | grepl("\\*", i)) {
        rho[i, 1] <- 1
      } else {
        # Calculate rhoA
        rho[i, 1] <- compute_construct_rhoA(weights, mmMatrix, construct = i, obsData)
      }
    }
  }
  return(rho)
}
# End rho_A function

# RhoC and AVE
# Dillon-Goldstein's Rho as per: Dillon, W. R, and M. Goldstein. 1987. Multivariate Analysis: Methods
# and Applications. Biometrical Journal 29 (6).
# Average Variance Extracted as per:  Fornell, C. and D. F. Larcker (February 1981). Evaluating
# structural equation models with unobservable variables and measurement error, Journal of Marketing Research, 18, pp. 39-5
rhoC_AVE <- function(x, ...) {
  UseMethod("rhoC_AVE", x)
}

rhoC_AVE.pls_model <- rhoC_AVE.boot_seminr_model <- function(pls_model){
  dgr <- matrix(NA, nrow=length(pls_model$constructs), ncol=2)
  rownames(dgr) <- pls_model$constructs
  colnames(dgr) <- c("rhoC", "AVE")
  for(i in pls_model$constructs){
    loadings <- pls_model$outer_loadings[, i]
    ind <- which(loadings != 0)
    if(measure_mode(i, pls_model$mmMatrix) %in% c("A", "B", "HOCA", "HOCB")) {
      if(length(ind) == 1) {
        dgr[i, 1:2] <- 1
      } else {
        lambdas <- loadings[ind]
        dgr[i, 1] <- compute_rhoC(lambdas)
        dgr[i, 2] <- compute_AVE(lambdas)
      }
    } else {
      lambdas <- loadings[ind]
      dgr[i, 1] <- NA
      dgr[i, 2] <- compute_AVE(lambdas)
    }
  }
  return(dgr)
}

# Assumes factor loadings are in model:
# lavaan::inspect(fit,what="std")$lambda
rhoC_AVE.cbsem_model <- rhoC_AVE.cfa_model <-  function(seminr_model) {
  dgr <- matrix(NA, nrow=length(seminr_model$constructs), ncol=2)
  rownames(dgr) <- seminr_model$constructs
  colnames(dgr) <- c("rhoC", "AVE")
  for(i in seminr_model$constructs) {
    loadings <- seminr_model$factor_loadings[, i]
    ind <- which(loadings != 0)
    if(length(ind) == 1) {
      dgr[i, 1:2] <- 1
    } else {
      lambdas <- loadings[ind]
      dgr[i, 1] <- compute_rhoC(lambdas)
      dgr[i, 2] <- compute_AVE(lambdas)
    }
  }
  return(dgr)
}

cron_alpha <- function(cov_mat) {
  k <- nrow(cov_mat)
  cov_i <- sum(diag(cov_mat))
  # mean_cov <- mean(cov_mat[upper.tri(cov_mat)])
  # alpha <- (k^2*mean_cov)/sum(cov_mat)
  alpha <- (k/(k-1))*(1 - (cov_i/sum(cov_mat)))
  return(alpha)
}

cronbachs_alpha <- function(seminr_model) {
  alpha_vec <- c()
  for (i in seminr_model$constructs) {
    items <- seminr_model$mmMatrix[seminr_model$mmMatrix[,"construct"] == i,"measurement"]
    if (length(items) > 1) {
      cov_mat <- stats::cor(seminr_model$data, seminr_model$data)[items, items]
      alpha_vec[[i]] <- cron_alpha(cov_mat)
    } else {
      alpha_vec[[i]] <- 1
    }
  }
  return(unlist(alpha_vec))
}
