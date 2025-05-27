#' seminr bootstrap_model Function
#'
#' The \code{seminr} package provides a natural syntax for researchers to describe PLS
#' structural equation models.
#' \code{bootstrap_model} provides the verb for bootstrapping a pls model from the model
#' parameters and data.
#'
#' @param seminr_model A fully estimated model with associated data, measurement model and structural model
#'
#' @param nboot A parameter specifying the number of bootstrap iterations to perform, default
#'        value is 500. If 0 then no bootstrapping is performed.
#'
#' @param cores A parameter specifying the maximum number of cores to use in the parallelization.
#'
#' @param seed A parameter to specify the seed for reproducibility of results. Default is NULL.
#'
#' @param ... A list of parameters passed on to the estimation method.
#'
#' @return A list of the estimated parameters for the bootstrapped model including:
#'  \item{boot_paths}{An array of the `nboot` estimated bootstrap sample path coefficient matrices.}
#'  \item{boot_loadings}{An array of the `nboot` estimated bootstrap sample item loadings matrices.}
#'  \item{boot_weights}{An array of the `nboot` estimated bootstrap sample item weights matrices.}
#'  \item{boot_HTMT}{An array of the `nboot` estimated bootstrap sample model HTMT matrices.}
#'  \item{boot_total_paths}{An array of the `nboot` estimated bootstrap sample model total paths matrices.}
#'  \item{paths_descriptives}{A matrix of the bootstrap path coefficients and standard deviations.}
#'  \item{loadings_descriptives}{A matrix of the bootstrap item loadings and standard deviations.}
#'  \item{weights_descriptives}{A matrix of the bootstrap item weights and standard deviations.}
#'  \item{HTMT_descriptives}{A matrix of the bootstrap model HTMT and standard deviations.}
#'  \item{total_paths_descriptives}{A matrix of the bootstrap model total paths and standard deviations.}
#'
#' @usage
#' bootstrap_model(seminr_model, nboot = 500, cores = NULL, seed = NULL, ...)
#'
#' @seealso \code{\link{relationships}} \code{\link{constructs}} \code{\link{paths}} \code{\link{interaction_term}}
#'
#' @references Hair, J. F., Hult, G. T. M., Ringle, C. M., and Sarstedt, M. (2017). A Primer on Partial Least Squares
#'  Structural Equation Modeling (PLS-SEM), 2nd Ed., Sage: Thousand Oaks.
#'
#' @examples
#' data(mobi)
#' # seminr syntax for creating measurement model
#' mobi_mm <- constructs(
#'   composite("Image",        multi_items("IMAG", 1:5)),
#'   composite("Expectation",  multi_items("CUEX", 1:3)),
#'   composite("Value",        multi_items("PERV", 1:2)),
#'   composite("Satisfaction", multi_items("CUSA", 1:3)),
#'   interaction_term(iv = "Image", moderator = "Expectation", method = orthogonal),
#'   interaction_term(iv = "Image", moderator = "Value", method = orthogonal)
#' )
#'
#' # structural model: note that name of the interactions construct should be
#' #  the names of its two main constructs joined by a '*' in between.
#' mobi_sm <- relationships(
#'   paths(to = "Satisfaction",
#'         from = c("Image", "Expectation", "Value",
#'                  "Image*Expectation", "Image*Value"))
#' )
#'
#' seminr_model <- estimate_pls(data = mobi,
#'                              measurement_model = mobi_mm,
#'                              structural_model = mobi_sm)
#'
#' # Load data, assemble model, and bootstrap
#' boot_seminr_model <- bootstrap_model(seminr_model = seminr_model,
#'                                      nboot = 50, cores = 2, seed = NULL)
#'
#' summary(boot_seminr_model)
#' @export
bootstrap_model <- function(seminr_model, nboot = 500, cores = NULL, seed = NULL, ...) {
  out <- tryCatch(
    {
      # Bootstrapping for significance as per Hair, J. F., Hult, G. T. M., Ringle, C. M., and Sarstedt, M. (2017). A Primer on
      # Partial Least Squares Structural Equation Modeling (PLS-SEM), 2nd Ed., Sage: Thousand Oaks.
      message("Bootstrapping model using seminr...")

      # prepare parameters for cluster export (model parameters)
      d <- seminr_model$rawdata
      measurement_model <- seminr_model$measurement_model
      structural_model <- seminr_model$smMatrix
      inner_weights <- seminr_model$inner_weights
      missing_value <- seminr_model$settings$missing_value
      maxIt <- seminr_model$settings$maxIt
      stopCriterion <- seminr_model$settings$stopCriterion
      missing <- seminr_model$settings$missing


      # Initialize the cluster
      suppressWarnings(ifelse(is.null(cores), cl <- parallel::makeCluster(parallel::detectCores(), setup_strategy = "sequential"), cl <- parallel::makeCluster(cores, setup_strategy = "sequential")))

      # Function to generate random samples with replacement
      getRandomIndex <- function(d) {return(sample.int(nrow(d), replace = TRUE))}

      # Check for and create random seed if NULL
      if (is.null(seed)) {seed <- sample.int(100000, size = 1)}

      # Export variables and functions to cluster
      parallel::clusterExport(cl=cl, varlist=c("measurement_model",
                                               "structural_model",
                                               "inner_weights",
                                               "getRandomIndex",
                                               "d",
                                               "HTMT",
                                               "seed",
                                               "total_effects",
                                               "missing_value",
                                               "maxIt",
                                               "stopCriterion",
                                               "missing"), envir=environment())

      # Calculate the expected nrow of the bootmatrix
      length <- 3*nrow(seminr_model$path_coef)^2 + 2*nrow(seminr_model$outer_loadings)*ncol(seminr_model$outer_loadings)

      # Function to get PLS estimate results
      getEstimateResults <- function(i, d = d, length) {
        set.seed(seed + i)
        # plsc-bootstrap
        tryCatch({
          boot_model <- seminr::estimate_pls(data = d[getRandomIndex(d),],
                                             measurement_model,
                                             structural_model,
                                             inner_weights = inner_weights)
          boot_htmt <- HTMT(boot_model)
          boot_total <- total_effects(boot_model$path_coef)
          return(as.matrix(c(c(boot_model$path_coef),
                             c(boot_model$outer_loadings),
                             c(boot_model$outer_weights),
                             c(boot_htmt),
                             c(boot_total))))
          },
          error = function(cond) {
            message("Bootstrapping encountered an ERROR: ")
            message(cond)
            return(rep(NA, length))
          },
          warning = function(cond) {
            message("Bootstrapping encountered an ERROR: ")
            message(cond)
            return(rep(NA, length))
          }
        )
      }

      # Bootstrap the estimates
      utils::capture.output(bootmatrix <- parallel::parSapply(cl, 1:nboot, getEstimateResults, d, length))

      # Clean the NAs and report the NAs
      bootmatrix <- bootmatrix[,!is.na(bootmatrix[1,])]
      fails <- nboot - ncol(bootmatrix)
      nboot <- nboot - fails
      if (fails > 0) {
        message(paste("Bootstrapping encountered a WARNING: ", fails, "bootstrap iterations failed to converge (possibly due to PLSc). \nThese failed iterations are excluded from the reported bootstrap statistics."))
      }


      # Collect means and sds for all estimates from bootmatrix
      means <- apply(bootmatrix,1,mean)
      sds <- apply(bootmatrix,1,stats::sd)

      # Create the matrix and array of bootstrapped paths ----

      # Collect the dimensions of the path_coef matrix
      path_cols <- ncol(seminr_model$path_coef)
      path_rows <- nrow(seminr_model$path_coef)

      # Identify start and end points for path coef data
      start <- 1
      end <- (path_cols*path_rows)

      # Create the array of bootstrap paths
      boot_paths <- array(bootmatrix[start:end,1:nboot], dim = c(path_rows, path_cols, nboot), dimnames = list(rownames(seminr_model$path_coef), colnames(seminr_model$path_coef),1:nboot))

      # Create the summary matrices of means and sds for path coefs
      paths_means <- matrix(means[start:end], nrow = path_rows, ncol = path_cols)
      paths_sds <- matrix(sds[start:end], nrow = path_rows, ncol = path_cols)

      # Create the bootstrapped paths matrix (take care to not be stranded with a single column/row vector)
      paths_descriptives <- cbind(seminr_model$path_coef, paths_means, paths_sds)

      # Clean the empty paths (take care to not be stranded with a single column/row vector)
      filled_cols <- apply(paths_descriptives != 0, 2, any, na.rm=TRUE)
      filled_rows <- apply(paths_descriptives != 0, 1, any, na.rm=TRUE)
      paths_descriptives <- subset(paths_descriptives, filled_rows, filled_cols)

      # Get the names of endogenous variables
      if (length(all_endogenous(structural_model)) == 1) {
        dependant <- all_endogenous(structural_model)
      } else {
        dependant <- colnames(paths_descriptives[, 1:length(all_endogenous(structural_model)), drop = F])
      }

      # Construct the vector of column names
      col_names <- c()
      # Clean the column names
      for (parameter in c("PLS Est.", "Boot Mean", "Boot SD")) {
        for (i in 1:length(dependant)) {
          col_names <- c(col_names, paste(dependant[i], parameter, sep = " "))
        }
      }

      # Assign column names
      colnames(paths_descriptives) <- col_names

      # Create the matrix and array of bootstrapped loadings ----

      # Collect the dimensions of the path_coef matrix
      mm_cols <- ncol(seminr_model$outer_loadings)
      mm_rows <- nrow(seminr_model$outer_loadings)

      # Identify start and end points for path coef data
      start <- end+1
      end <- start+(mm_cols*mm_rows)-1

      # create the array of bootstrapped loadings
      boot_loadings <- array(bootmatrix[start:end,1:nboot], dim = c(mm_rows, mm_cols, nboot), dimnames = list(rownames(seminr_model$outer_loadings), colnames(seminr_model$outer_loadings),1:nboot))

      # Create the summary matrices of means and sds for loadings
      loadings_means <- matrix(means[start:end], nrow = mm_rows, ncol = mm_cols)
      loadings_sds <- matrix(sds[start:end], nrow = mm_rows, ncol = mm_cols)

      # Subset paths matrix (take care to not be stranded with a single column/row vector)
      loadings_descriptives <- cbind(seminr_model$outer_loadings, loadings_means, loadings_sds)

      # Construct the vector of column names 2 for outer model
      col_names2 <- c()
      # Clean the column names
      for (parameter in c("PLS Est.", "Boot Mean", "Boot SD")) {
        for(i in colnames(seminr_model$outer_loadings)) {
          col_names2 <- c(col_names2, paste(i, parameter, sep = " "))
        }
      }

      # Assign column names to loadings matrix
      colnames(loadings_descriptives) <- col_names2

      # Identify start and end points for path weights data
      start <- end+1
      end <- start+(mm_cols*mm_rows)-1

      # create the array of bootstrapped weights
      boot_weights <- array(bootmatrix[start:end,1:nboot], dim = c(mm_rows, mm_cols, nboot), dimnames = list(rownames(seminr_model$outer_loadings), colnames(seminr_model$outer_loadings),1:nboot))

      # Create the summary matrices of means and sds
      weights_means <- matrix(means[start:end], nrow = mm_rows, ncol = mm_cols)
      weights_sds <- matrix(sds[start:end], nrow = mm_rows, ncol = mm_cols)

      # create the weights matrix (take care to not be stranded with a single column/row vector)
      weights_descriptives <- cbind(seminr_model$outer_weights, weights_means, weights_sds)

      # Assign column names to weights matrix
      colnames(weights_descriptives) <- col_names2

      # Collect HTMT matrix
      HTMT_matrix <- HTMT(seminr_model)

        # Identify start and end points for HTMT data
        start <- end+1
        end <- start+(ncol(HTMT_matrix)*nrow(HTMT_matrix))-1

        # Collect the array of bootstrapped HTMT
        boot_HTMT <- array(bootmatrix[start:end,1:nboot], dim = c(nrow(HTMT_matrix), ncol(HTMT_matrix), nboot), dimnames = list(rownames(HTMT_matrix), colnames(HTMT_matrix),1:nboot))

        # Collect the matrices of means and sds for HTMT
        htmt_means <- matrix(means[start:end], nrow = nrow(HTMT_matrix), ncol = ncol(HTMT_matrix))
        htmt_sds <- matrix(sds[start:end], nrow = nrow(HTMT_matrix), ncol = ncol(HTMT_matrix))

      # create HTMT matrix (take care to not be stranded with a single column/row vector)
      HTMT_descriptives <- cbind(HTMT_matrix, htmt_means, htmt_sds)

      # Construct the vector of column names 3 for HTMT
      col_names3 <- c()
      # Clean the column names
      for (parameter in c("PLS Est.", "Boot Mean", "Boot SD")) {
        for(i in colnames(HTMT_matrix)) {
          col_names3 <- c(col_names3, paste(i, parameter, sep = " "))
        }
      }
      # Get boot_HTMT column names
      colnames(HTMT_descriptives) <- col_names3

      # Subset total paths matrix (take care to not be stranded with a single column/row vector)
      total_matrix <- total_effects(seminr_model$path_coef)
      start <- end+1
      end <- start+(path_cols*path_rows)-1

      # Collect the array of bootstrapped total paths
      boot_total_paths <- array(bootmatrix[start:end,1:nboot], dim = c(path_rows, path_cols, nboot), dimnames = list(rownames(total_matrix), colnames(total_matrix),1:nboot))

      # Collect the matrices for means and sds
      total_means <- matrix(means[start:end], nrow = path_rows, ncol = path_cols)
      total_sds <- matrix(sds[start:end], nrow = path_rows, ncol = path_cols)


      # Subset paths matrix (take care to not be stranded with a single column/row vector)
      total_paths_descriptives <- cbind(total_matrix, total_means, total_sds)

      # Clean the empty paths (take care to not be stranded with a single column/row vector)
      filled_cols <- apply(total_paths_descriptives != 0, 2, any, na.rm=TRUE)
      filled_rows <- apply(total_paths_descriptives != 0, 1, any, na.rm=TRUE)
      total_paths_descriptives <- subset(total_paths_descriptives, filled_rows, filled_cols)

      # Assign column names
      colnames(total_paths_descriptives) <- col_names

      # Change the class of the descriptives objects
      class(paths_descriptives) <- append(class(paths_descriptives), "table_output")
      class(loadings_descriptives) <- append(class(loadings_descriptives), "table_output")
      class(weights_descriptives) <- append(class(weights_descriptives), "table_output")
      class(HTMT_descriptives) <- append(class(HTMT_descriptives), "table_output")
      class(total_paths_descriptives) <- append(class(total_paths_descriptives), "table_output")

      # Add the bootstrap matrix to the seminr_model object
      seminr_model$boot_paths <- boot_paths
      seminr_model$boot_loadings <- boot_loadings
      seminr_model$boot_weights <- boot_weights
      seminr_model$boot_HTMT <- boot_HTMT
      seminr_model$boot_total_paths <- boot_total_paths
      seminr_model$paths_descriptives <- paths_descriptives
      seminr_model$loadings_descriptives <- loadings_descriptives
      seminr_model$weights_descriptives <- weights_descriptives
      seminr_model$HTMT_descriptives <- HTMT_descriptives
      seminr_model$total_paths_descriptives <- total_paths_descriptives
      seminr_model$boots <- nboot
      seminr_model$seed <- seed
      class(seminr_model) <- c("boot_seminr_model", "seminr_model")
      message("SEMinR Model successfully bootstrapped")
      return(seminr_model)
    },
    error = function(cond) {
      message("Bootstrapping encountered this ERROR: ")
      message(cond)
      seminr_model <- NULL
    },
    warning = function(cond) {
      message("Bootstrapping encountered this WARNING:")
      message(cond)
      seminr_model <- NULL
    },
    finally = {
      parallel::stopCluster(cl)
      return(seminr_model)
    }
  )
}
