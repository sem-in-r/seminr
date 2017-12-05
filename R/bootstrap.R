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
#' # interaction factors must be created after the measurement model is defined
#' mobi_xm <- interactions(
#'   interaction_ortho("Image", "Expectation"),
#'   interaction_ortho("Image", "Value")
#' )
#'
#' # structural model: note that name of the interactions factor should be
#' #  the names of its two main factors joined by a '.' in between.
#' mobi_sm <- relationships(
#'   paths(to = "Satisfaction",
#'         from = c("Image", "Expectation", "Value",
#'                  "Image.Expectation", "Image.Value"))
#' )
#'
#' seminr.model <- estimate_pls(data = mobi,
#'                              measurement_model = mobi_mm,
#'                              interactions = mobi_xm,
#'                              structural_model = mobi_sm)
#'
#' # Load data, assemble model, and bootstrap using simplePLS
#' boot.seminr.model <- bootstrap_model(seminr_model = seminr.model,
#'                                      nboot = 100, cores = 2)
#'
#' print_paths(boot.seminr.model)
#' @export
bootstrap_model <- function(seminr_model, nboot = 500, cores = NULL,...) {
  cat("Bootstrapping model using simplePLS...\n")

  # prepare parameters for cluster export (model parameters)
  interactions = seminr_model$mobi_xm
  d <- seminr_model$rawdata
  measurement_model <- seminr_model$mmMatrix
  structural_model <- seminr_model$smMatrix
  inner_weights <- seminr_model$inner_weights

  if (nboot > 0) {
    # Initialize the cluster
    ifelse(is.null(cores), cl <- parallel::makeCluster(parallel::detectCores()), cl <- parallel::makeCluster(cores))

    # Initialize the Estimates Matrix
    bootstrapMatrix <- seminr_model$path_coef
    cols <- ncol(bootstrapMatrix)
    rows <- nrow(bootstrapMatrix)

    # Function to generate random samples with replacement
    getRandomIndex <- function(d) {return(sample.int(nrow(d),replace = TRUE))}

    # Export variables and functions to cluster
    parallel::clusterExport(cl=cl, varlist=c("measurement_model", "interactions", "structural_model","inner_weights","getRandomIndex","d"), envir=environment())

    # Function to get PLS estimate results
    getEstimateResults <- function(i, d = d) {
      return(seminr::estimate_pls(data = d[getRandomIndex(d),],
                          measurement_model,interactions,structural_model,inner_weights)$path_coef)
    }

    # Bootstrap the estimates
    utils::capture.output(bootmatrix <- parallel::parSapply(cl,1:nboot,getEstimateResults, d))

    # Add the columns for bootstrap mean and standard error
    bootstrapMatrix <- cbind(bootstrapMatrix,matrix(apply(bootmatrix,1,mean),nrow = rows, ncol = cols))
    bootstrapMatrix <- cbind(bootstrapMatrix,matrix(apply(bootmatrix,1,stats::sd),nrow = rows, ncol = cols))

    # Clean the empty paths
    bootstrapMatrix <- bootstrapMatrix[, colSums(bootstrapMatrix != 0, na.rm = TRUE) > 0]
    bootstrapMatrix <- bootstrapMatrix[rowSums(bootstrapMatrix != 0, na.rm = TRUE) > 0,]

    # Get the number of DVs
    if (length(unique(structural_model[,"target"])) == 1) {
      dependant <- unique(structural_model[,"target"])
    } else {
      dependant <- colnames(bootstrapMatrix[,1:length(unique(structural_model[,"target"]))])
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
    colnames(bootstrapMatrix) <- colnames

    # Add the bootstrap matrix to the simplePLS object
    seminr_model$bootstrapMatrix <- bootstrapMatrix
    parallel::stopCluster(cl)
  }
  seminr_model$boots <- nboot
  class(seminr_model) <- "boot_seminr_model"
  return(seminr_model)
}
