#' seminr rho_A Function
#'
#' The \code{rho_A} function calculates the rho_A reliability indices for each construct. For
#' formative constructs, the index is set to 1.
#'
#' @param seminr_model A \code{seminr_model} containing the estimated seminr model.
#'
#' @param constructs A vector containing the names of the constructs to calculate rhoA for.
#'
#' @return A matrix containing the rhoA metric for each construct.
#'
#' @usage
#' rho_A(seminr_model, constructs)
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
#' rho_A(mobi_pls, mobi_pls$constructs)
#' @export
# rho_A as per Dijkstra, T. K., & Henseler, J. (2015). Consistent Partial Least Squares Path Modeling, 39(X).
rho_A <- function(seminr_model, constructs) {
  # get weights for each construct
  weights <- seminr_model$outer_weights
  # get the mmMatrix and smMatrix
  mmMatrix <- seminr_model$mmMatrix
  obsData <- seminr_model$data

  # Create rho_A holder matrix
  rho <- matrix(, nrow = length(constructs), ncol = 1, dimnames = list(constructs, c("rhoA")))

  for (i in rownames(rho))  {
    #If the measurement model is Formative assign rhoA = 1
    if(mmMatrix[mmMatrix[, "construct"]==i, "type"][1] %in% c("B", "HOCB")){ #| mmMatrix[mmMatrix[, "construct"]==i, "type"][1]=="A"){
      rho[i, 1] <- 1
    }
    #If the measurement model is Reflective Calculate RhoA
    if(mmMatrix[mmMatrix[, "construct"]==i, "type"][1] %in% c("C", "A", "HOCA", "UNIT")) {#| mmMatrix[mmMatrix[, "construct"]==i, "type"][1]=="A"|){
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

#' seminr rhoC_AVE() function
#'
#' Get rhoC and AVE for a CFA model estimated with \code{estimate_pls}, \code{estimate_cbsem} or \code{estimate_cfa}.
#' Dillon-Goldstein's Rho as per: Dillon, W. R, and M. Goldstein. 1987. Multivariate Analysis: Methods
#' and Applications. Biometrical Journal 29 (6).
#' Average Variance Extracted as per:  Fornell, C. and D. F. Larcker (February 1981). Evaluating
#' structural equation models with unobservable variables and measurement error, Journal of Marketing Research, 18, pp. 39-5
#'
#' @param x Estimated \code{seminr_model} object.
#'
#' @param constructs Vector containing the names of the constructs to calculate rhoC and AVE for; if NULL, all constructs are used.
#'
#' @return A matrix containing the rhoC and AVE metrics for each construct.
#'
#' @export
rhoC_AVE <- function(x, constructs = NULL) {
  UseMethod("rhoC_AVE")
}

#' @export
rhoC_AVE.pls_model <- rhoC_AVE.boot_seminr_model <- function(x, constructs = NULL) {
  pls_model <- x
  if (is.null(constructs)) {
    constructs <- pls_model$constructs
  }

  dgr <- matrix(NA, nrow=length(constructs), ncol=2)
  rownames(dgr) <- constructs
  colnames(dgr) <- c("rhoC", "AVE")
  for(i in constructs){
    loadings <- pls_model$outer_loadings[, i]
    ind <- which(loadings != 0)
    if(measure_mode(i, pls_model$mmMatrix) %in% c("A", "B", "HOCA", "HOCB", "C", "UNIT")) {
      if(length(ind) == 1) {
        dgr[i, 1:2] <- 1
      } else {
        lambdas <- loadings[ind]
        dgr[i, 1] <- compute_rhoC(lambdas)
        dgr[i, 2] <- compute_AVE(lambdas)
      }
    }
  }
  return(dgr)
}

#' @export
rhoC_AVE.cbsem_model <- function(x, constructs = NULL) {
  # Assumes factor loadings are in model: lavaan::inspect(fit,what="std")$lambda
  cbsem_model <- x
  if (is.null(constructs)) {
    constructs <- cbsem_model$constructs
  }

  dgr <- matrix(NA, nrow=length(constructs), ncol=2)
  rownames(dgr) <- constructs
  colnames(dgr) <- c("rhoC", "AVE")
  for(i in constructs) {
    loadings <- cbsem_model$factor_loadings[, i]
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

#' @export
rhoC_AVE.cfa_model <- function(x, constructs = NULL) {
  cfa_model <- x
  if (is.null(constructs)) {
    constructs <- cfa_model$constructs
  }

  dgr <- matrix(NA, nrow=length(constructs), ncol=2)
  rownames(dgr) <- constructs
  colnames(dgr) <- c("rhoC", "AVE")
  for(i in constructs) {
    loadings <- cfa_model$factor_loadings[, i]
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
  alpha <- (k/(k-1))*(1 - (cov_i/sum(cov_mat)))
  return(alpha)
}

cronbachs_alpha <- function(seminr_model, constructs) {
  alpha_vec <- c()
  for (i in constructs) {
    items <- seminr_model$mmMatrix[seminr_model$mmMatrix[,"construct"] == i,"measurement"]
    if (length(items) > 1) {
      cov_mat <- stats::cor(seminr_model$data, seminr_model$data)[items, items]
      alpha_vec[[i]] <- cron_alpha(cov_mat)
    } else {
      alpha_vec[[i]] <- 1
    }
  }
  return(matrix(unlist(alpha_vec), ncol = 1))
}
