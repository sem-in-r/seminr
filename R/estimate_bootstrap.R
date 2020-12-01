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
      cat("Bootstrapping model using seminr...\n")

      # prepare parameters for cluster export (model parameters)
      d <- seminr_model$rawdata
      measurement_model <- seminr_model$measurement_model
      structural_model <- seminr_model$smMatrix
      inner_weights <- seminr_model$inner_weights

      if (nboot > 0) {
        # Initialize the cluster
        suppressWarnings(ifelse(is.null(cores), cl <- parallel::makeCluster(parallel::detectCores()), cl <- parallel::makeCluster(cores)))

        # Initialize the Estimates Matrix
        bootstrapMatrix <- rbind(seminr_model$path_coef, seminr_model$outer_loadings, seminr_model$outer_weights, HTMT(seminr_model), total_effects(seminr_model$path_coef))
        cols <- ncol(bootstrapMatrix)
        rows <- nrow(bootstrapMatrix)

        # Function to generate random samples with replacement
        getRandomIndex <- function(d) {return(sample.int(nrow(d), replace = TRUE))}

        # Check for and create random seed if NULL
        if (is.null(seed)) {seed <- sample.int(100000, size = 1)}

        # Export variables and functions to cluster
        parallel::clusterExport(cl=cl, varlist=c("measurement_model", "structural_model", "inner_weights", "getRandomIndex", "d", "HTMT", "seed","total_effects"), envir=environment())

        # Function to get PLS estimate results
        getEstimateResults <- function(i, d = d) {
          set.seed(seed + i)
          boot_model <- seminr::estimate_pls(data = d[getRandomIndex(d),],
                               measurement_model,
                               structural_model,
                               inner_weights)
          boot_htmt <- HTMT(boot_model)
          boot_total <- total_effects(boot_model$path_coef)
          return(rbind(boot_model$path_coef, boot_model$outer_loadings, boot_model$outer_weights, boot_htmt, boot_total))
        }

        # Bootstrap the estimates
        utils::capture.output(bootmatrix <- parallel::parSapply(cl, 1:nboot, getEstimateResults, d))

        # Add the columns for bootstrap mean and standard error
        bootstrapMatrix <- cbind(bootstrapMatrix, matrix(apply(bootmatrix,1,mean), nrow = rows, ncol = cols))
        bootstrapMatrix <- cbind(bootstrapMatrix, matrix(apply(bootmatrix,1,stats::sd), nrow = rows, ncol = cols))

        # Subset paths matrix (take care to not be stranded with a single column/row vector)
        paths_descriptives <- matrix(bootstrapMatrix[1:(cols-1), c(1:(3*cols))], nrow=(cols-1), ncol=(3*cols))
        dimnames(paths_descriptives) <- list(
          rownames(bootstrapMatrix)[1:(cols-1)],
          colnames(bootstrapMatrix)[1:(3*cols)]
        )

        # Clean the empty paths (take care to not be stranded with a single column/row vector)
        filled_cols <- apply(paths_descriptives != 0, 2, any, na.rm=TRUE)
        filled_rows <- apply(paths_descriptives != 0, 1, any, na.rm=TRUE)
        paths_descriptives <- subset(paths_descriptives, filled_rows, filled_cols)

        # Get the number of DVs
        if (length(unique(structural_model[,"target"])) == 1) {
          dependant <- unique(structural_model[,"target"])
        } else {
          dependant <- colnames(paths_descriptives[, 1:length(unique(structural_model[,"target"]))])
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

        # collect loadings matrix
        loadings_descriptives <- bootstrapMatrix[(cols+1):((cols)+nrow(seminr_model$outer_loadings)), c(1:(3*cols))]


        # Construct the vector of column names 2
        col_names2 <- c()
        # Clean the column names
        for (parameter in c("PLS Est.", "Boot Mean", "Boot SD")) {
          for(i in seminr_model$constructs) {
            col_names2 <- c(col_names2, paste(i, parameter, sep = " "))
          }
        }

        # Assign column names to loadings
        colnames(loadings_descriptives) <- col_names2

        # collect weights matrix
        weights_descriptives <- bootstrapMatrix[((cols+1)+nrow(seminr_model$outer_loadings)):((cols)+(2*nrow(seminr_model$outer_loadings))), c(1:(3*cols))]

        # Assign column names to weights
        colnames(weights_descriptives) <- col_names2

        # Collect HTMT matrix
        HTMT_descriptives <- bootstrapMatrix[((cols+1)+(2*nrow(seminr_model$outer_loadings))):((cols+cols)+(2*nrow(seminr_model$outer_loadings))), c(1:(3*cols))]

        # Clean the empty paths
        #boot_HTMT <- boot_HTMT[, colSums(boot_HTMT != 0, na.rm = TRUE) > 0]
        #boot_HTMT <- boot_HTMT[rowSums(boot_HTMT != 0, na.rm = TRUE) > 0,]

        # Get boot_HTMT column names
        colnames(HTMT_descriptives) <- col_names2

        # Subset total paths matrix (take care to not be stranded with a single column/row vector)
        total_paths_descriptives <- matrix(bootstrapMatrix[((2*cols+1)+2*nrow(seminr_model$outer_loadings)):((3*cols)+(2*nrow(seminr_model$outer_loadings))), c(1:(3*cols))], nrow=(cols), ncol=(3*cols))
        dimnames(total_paths_descriptives) <- list(
          rownames(bootstrapMatrix)[1:(cols)],
          colnames(bootstrapMatrix)[1:(3*cols)]
        )

        # Clean the empty paths (take care to not be stranded with a single column/row vector)
        filled_cols <- apply(total_paths_descriptives != 0, 2, any, na.rm=TRUE)
        filled_rows <- apply(total_paths_descriptives != 0, 1, any, na.rm=TRUE)
        total_paths_descriptives <- subset(total_paths_descriptives, filled_rows, filled_cols)

        # Assign column names
        colnames(total_paths_descriptives) <- col_names

        # Create an array of results in bootmatrix
        bootarray <- array(bootmatrix, dim = c(nrow(bootstrapMatrix), length(seminr_model$constructs),nboot), dimnames = list(c(rownames(bootstrapMatrix)), c(seminr_model$constructs), c(1:nboot)))

        # Create arrays of bootstrapped path, loadings, weights, HTMT coefficients results
        boot_paths <- bootarray[1:length(seminr_model$constructs), , 1:nboot]
        boot_loadings <- bootarray[(length(seminr_model$constructs)+1):((length(seminr_model$constructs))+length(seminr_model$mmVariables)), , 1:nboot]
        boot_weights <- bootarray[(length(seminr_model$constructs)+length(seminr_model$mmVariables)+1):((length(seminr_model$constructs))+2*length(seminr_model$mmVariables)), , 1:nboot]
        boot_HTMT <- bootarray[((length(seminr_model$constructs))+2*length(seminr_model$mmVariables)+1):((2*length(seminr_model$constructs))+2*length(seminr_model$mmVariables)), , 1:nboot]
        boot_total_paths <- bootarray[((2*length(seminr_model$constructs))+2*length(seminr_model$mmVariables)+1):((3*length(seminr_model$constructs))+2*length(seminr_model$mmVariables)), , 1:nboot]

        # Change the class of the descriptives objects
        class(paths_descriptives) <- append(class(paths_descriptives), "table_output")
        class(loadings_descriptives) <- append(class(loadings_descriptives), "table_output")
        class(weights_descriptives) <- append(class(weights_descriptives), "table_output")
        class(HTMT_descriptives) <- append(class(HTMT_descriptives), "table_output")
        class(total_paths_descriptives) <- append(class(total_paths_descriptives), "table_output")
        parallel::stopCluster(cl)
      }

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
      class(seminr_model) <- "boot_seminr_model"
      cat("SEMinR Model successfully bootstrapped")
      return(seminr_model)
    },
    error = function(cond) {
      message("Bootstrapping encountered this ERROR: ")
      message(cond)
      parallel::stopCluster(cl)
      return(NULL)
    },
    warning = function(cond) {
      message("Bootstrapping encountered this WARNING:")
      message(cond)
      parallel::stopCluster(cl)
      return(NULL)
    },
    finally = {
      #
    }
  )
}
