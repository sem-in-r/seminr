#' seminr bootstrap_model Function
#'
#' The \code{seminr} package provides a natural syntax for researchers to describe PLS
#' structural equation models. \code{seminr} is compatible with simplePLS.
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
#' @param ... A list of parameters passed on to the estimation method (e.g., \code{simplePLS}).
#'
#' @usage
#' bootstrap_model(seminr_model, nboot = 500, cores = NULL, ...)
#'
#' @seealso \code{\link{relationships}} \code{\link{constructs}} \code{\link{paths}} \code{\link{interactions}}
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
#'   composite("Satisfaction", multi_items("CUSA", 1:3))
#' )
#'
#' # interaction constructs must be created after the measurement model is defined
#' mobi_xm <- interactions(
#'   interaction_ortho("Image", "Expectation"),
#'   interaction_ortho("Image", "Value")
#' )
#'
#' # structural model: note that name of the interactions construct should be
#' #  the names of its two main constructs joined by a '.' in between.
#' mobi_sm <- relationships(
#'   paths(to = "Satisfaction",
#'         from = c("Image", "Expectation", "Value",
#'                  "Image*Expectation", "Image*Value"))
#' )
#'
#' seminr_model <- estimate_pls(data = mobi,
#'                              measurement_model = mobi_mm,
#'                              interactions = mobi_xm,
#'                              structural_model = mobi_sm)
#'
#' # Load data, assemble model, and bootstrap using simplePLS
#' boot_seminr_model <- bootstrap_model(seminr_model = seminr_model,
#'                                      nboot = 50, cores = 2)
#'
#' summary(boot_seminr_model)
#' @export
bootstrap_model <- function(seminr_model, nboot = 500, cores = NULL,...) {
  out <- tryCatch(
    {
      # Bootstrapping for significance as per Hair, J. F., Hult, G. T. M., Ringle, C. M., and Sarstedt, M. (2017). A Primer on
      # Partial Least Squares Structural Equation Modeling (PLS-SEM), 2nd Ed., Sage: Thousand Oaks.
      cat("Bootstrapping model using seminr...\n")

      # prepare parameters for cluster export (model parameters)
      interactions = seminr_model$mobi_xm
      d <- seminr_model$rawdata
      measurement_model <- seminr_model$raw_measurement_model
      structural_model <- seminr_model$smMatrix
      inner_weights <- seminr_model$inner_weights

      if (nboot > 0) {
        # Initialize the cluster
        suppressWarnings(ifelse(is.null(cores), cl <- parallel::makeCluster(parallel::detectCores()), cl <- parallel::makeCluster(cores)))

        # Initialize the Estimates Matrix
        bootstrapMatrix <- rbind(seminr_model$path_coef, seminr_model$outer_loadings,seminr_model$outer_weights,HTMT(seminr_model))
        cols <- ncol(bootstrapMatrix)
        rows <- nrow(bootstrapMatrix)

        # Function to generate random samples with replacement
        getRandomIndex <- function(d) {return(sample.int(nrow(d),replace = TRUE))}

        # Export variables and functions to cluster
        parallel::clusterExport(cl=cl, varlist=c("measurement_model", "interactions", "structural_model","inner_weights","getRandomIndex","d","HTMT"), envir=environment())

        # Function to get PLS estimate results
        getEstimateResults <- function(i, d = d) {
          boot_model <- seminr::estimate_pls(data = d[getRandomIndex(d),],
                               measurement_model,
                               interactions,
                               structural_model,
                               inner_weights)
          boot_htmt <- HTMT(boot_model)
          return(rbind(boot_model$path_coef, boot_model$outer_loadings, boot_model$outer_weights, boot_htmt))
        }

        # Bootstrap the estimates
        utils::capture.output(bootmatrix <- parallel::parSapply(cl,1:nboot,getEstimateResults, d))

        # Add the columns for bootstrap mean and standard error
        bootstrapMatrix <- cbind(bootstrapMatrix,matrix(apply(bootmatrix,1,mean),nrow = rows, ncol = cols))
        bootstrapMatrix <- cbind(bootstrapMatrix,matrix(apply(bootmatrix,1,stats::sd),nrow = rows, ncol = cols))

        # Create paths matrix
        boot_paths <- bootstrapMatrix[1:(cols-1),c(1:(3*cols))]

        # Clean the empty paths
        boot_paths <- boot_paths[, colSums(boot_paths != 0, na.rm = TRUE) > 0]
        boot_paths <- boot_paths[rowSums(boot_paths != 0, na.rm = TRUE) > 0,]

        # Get the number of DVs
        if (length(unique(structural_model[,"target"])) == 1) {
          dependant <- unique(structural_model[,"target"])
        } else {
          dependant <- colnames(boot_paths[,1:length(unique(structural_model[,"target"]))])
        }

        # Construct the vector of column names
        colnames<-c()
        # Clean the column names
        for (parameter in c("PLS Est.", "Boot Mean", "Boot SE")) {
          for(i in 1:length(dependant)) {
            colnames <- c(colnames, paste(dependant[i],parameter,sep = " "))
          }
        }

        # Assign column names
        colnames(boot_paths) <- colnames

        # collect loadings matrix
        boot_loadings <- bootstrapMatrix[(cols+1):((cols)+nrow(seminr_model$outer_loadings)),c(1:(3*cols))]


        # Construct the vector of column names 2
        colnames2<-c()
        # Clean the column names
        for (parameter in c("PLS Est.", "Boot Mean", "Boot SE")) {
          for(i in seminr_model$constructs) {
            colnames2 <- c(colnames2, paste(i,parameter,sep = " "))
          }
        }

        # Assign column names to loadings
        colnames(boot_loadings) <- colnames2

        # collect weights matrix
        boot_weights <- bootstrapMatrix[((cols+1)+nrow(seminr_model$outer_loadings)):((cols)+(2*nrow(seminr_model$outer_loadings))),c(1:(3*cols))]

        # Assign column names to weights
        colnames(boot_weights) <- colnames2

        # Collect HTMT matrix
        boot_HTMT <- bootstrapMatrix[((cols+1)+(2*nrow(seminr_model$outer_loadings))):((cols+cols)+(2*nrow(seminr_model$outer_loadings))),c(1:(3*cols))]

        # Clean the empty paths
        #boot_HTMT <- boot_HTMT[, colSums(boot_HTMT != 0, na.rm = TRUE) > 0]
        #boot_HTMT <- boot_HTMT[rowSums(boot_HTMT != 0, na.rm = TRUE) > 0,]

        # Get boot_HTMT column names
        colnames(boot_HTMT) <- colnames2



        parallel::stopCluster(cl)
      }

      # Add the bootstrap matrix to the simplePLS object
      seminr_model$bootstrapMatrix <- boot_paths
      seminr_model$boot_loadings <- boot_loadings
      seminr_model$boot_weights <- boot_weights
      seminr_model$boot_HTMT <- boot_HTMT
      seminr_model$boots <- nboot
      class(seminr_model) <- "boot_seminr_model"
      cat("SEMinR Model succesfully bootstrapped")
      return(seminr_model)
    },
    error=function(cond) {
      message("Bootstrapping encountered this ERROR: ")
      message(cond)
      parallel::stopCluster(cl)
      return(NULL)
    },
    warning=function(cond) {
      message("Bootstrapping encountered this WARNING:")
      message(cond)
      parallel::stopCluster(cl)
      return(NULL)
    },
    finally={
      #
    }
  )
}
