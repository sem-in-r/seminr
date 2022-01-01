#' Performs PLS-MGA to report significance of path differences between two subgroups of data
#'
#' @param pls_model SEMinR PLS model estimated on the full sample
#' @param condition logical vector of TRUE/FALSE indicating which rows of sample data are in group 1
#' @param nboot number of bootstrap resamples to use in PLS-MGA
#' @param ... any further parameters for bootstrapping (e.g., cores)
#'
#' @examples
#' mobi <- mobi
#'
#' #seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Quality",      multi_items("PERQ", 1:7)),
#'   composite("Value",        multi_items("PERV", 1:2)),
#'   composite("Satisfaction", multi_items("CUSA", 1:3)),
#'   composite("Complaints",   single_item("CUSCO")),
#'   composite("Loyalty",      multi_items("CUSL", 1:3))
#' )
#'
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
#'                          measurement_model = mobi_mm,
#'                          structural_model = mobi_sm,
#'                          missing = mean_replacement,
#'                          missing_value = NA)
#'
#' # Should usually use nboot ~2000 and don't specify cores for full parallel processing
#' mobi_mga <- estimate_pls_mga(mobi_pls, mobi$CUEX1 < 8, nboot=100, cores = 2)
#'
#' @export
estimate_pls_mga <- function(pls_model, condition, nboot = 2000, ...) {
  pls_data <- pls_model$rawdata

  # Given a beta report matrix (paths as rows) get estimates form a path_coef matrix
  path_estimate <- function(path, path_coef) {
    path_coef[path["source"], path["target"]]
  }

  # Get all path estimates of a given beta metrix from a given path_coef matrix
  # Typically used to apply on 3rd dimension of a 3x3 bootstrap paths array [from,to,boot]
  boot_paths <- function(path_coef, beta_df) {
    betas <- apply(beta_df, MARGIN=1, FUN=path_estimate, path_coef = path_coef)
  }

  # Allocate and Estimate Two Alternative Datasets + Models
  group1_data <- pls_data[condition, ]
  group2_data <- pls_data[!condition, ]

  message("Estimating and bootstrapping groups...")

  group1_model <- rerun(pls_model, data = group1_data)
  group2_model <- rerun(pls_model, data = group2_data)

  group1_boot <- bootstrap_model(seminr_model = group1_model, nboot = nboot, ...)
  group2_boot <- bootstrap_model(seminr_model = group2_model, nboot = nboot, ...)

  message("Computing similarity of groups")
  # Produce beta report matrix on all paths (as rows)
  beta <- as.data.frame(pls_model$smMatrix[,c("source", "target")])
  path_names <- do.call(paste0, cbind(beta["source"], " -> ", beta["target"]))
  rownames(beta) <- path_names
  beta$estimate <- apply(beta, MARGIN = 1, FUN=path_estimate, path_coef = pls_model$path_coef)

  beta$group1_beta <- apply(beta, MARGIN = 1, FUN=path_estimate, path_coef = group1_model$path_coef)
  beta$group2_beta <- apply(beta, MARGIN = 1, FUN=path_estimate, path_coef = group2_model$path_coef)

  beta_diff <- group1_model$path_coef - group2_model$path_coef
  beta$diff <- apply(beta, MARGIN = 1, FUN=path_estimate, path_coef = beta_diff)

  # Get bootstrapped paths for both groups
  boot1_betas <- t(apply(group1_boot$boot_paths, MARGIN=3, FUN=boot_paths, beta_df=beta))
  colnames(boot1_betas) <- path_names

  boot2_betas <- t(apply(group2_boot$boot_paths, MARGIN=3, FUN=boot_paths, beta_df=beta))
  colnames(boot2_betas) <- path_names

  # PLSc may not resolve in some bootstrap runs - limit bootstrap paths to resolved number of boots
  J <- min(dim(boot1_betas)[1], dim(boot2_betas)[1])
  if (J < nboot) {
    message(paste("NOTE: Using", J, "bootstrapped results of each group after removing inadmissible runs"))
  }
  boot1_betas <- boot1_betas[1:J,]
  boot2_betas <- boot2_betas[1:J,]


  # Insert bootstrap descriptives into beta matrix
  beta$group1_beta_mean <- apply(boot1_betas, MARGIN=2, FUN=mean)
  beta$group2_beta_mean <- apply(boot2_betas, MARGIN=2, FUN=mean)

  # beta$group1_beta_sd <- apply(boot1_betas, MARGIN=2, FUN=sd)
  # beta$group2_beta_sd <- apply(boot2_betas, MARGIN=2, FUN=sd)

  # Compute PLS-MGA p-value
  # see: Henseler, J., Ringle, C. M., & Sinkovics, R. R. (2009). The use of partial least squares path modeling in international marketing. In New challenges to international marketing. Emerald Group Publishing Limited.

  Theta <- function(s) {
    ifelse(s > 0, 1, 0)
  }

  beta_comparison <- function(i, beta, beta1_boots, beta2_boots) {
    for_all <- expand.grid(beta1_boots[,i], beta2_boots[,i])
    2*beta$group1_beta_mean[i] - for_all[,1] - 2*beta$group2_beta_mean[i] + for_all[,2]
  }

  pls_mga_p <- function(i, beta, beta1_boots, beta2_boots) {
    1 - (sum(Theta(beta_comparison(i, beta, beta1_boots, beta2_boots))) / J^2)
  }

  beta$pls_mga_p <- sapply(1:nrow(beta), FUN=pls_mga_p, beta=beta, beta1_boots=boot1_betas, beta2_boots=boot2_betas)

  class(beta) <- c("seminr_pls_mga", class(beta))
  beta
  # pls_mga_report <- round(beta[,-(1:2)], 2)
}
