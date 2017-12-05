test_of_power <- function(seminr_model, N, nboot = 500, distr = "normal",cores = 2) {
  refl <- c()
  for(i in 1:length(seminr_model$ltVariables)) {
    if (measure_mode(seminr_model$ltVariables[i],seminr_model$mmMatrix) == "A" | measure_mode(seminr_model$ltVariables[i],seminr_model$mmMatrix) == "C") {
      refl <- c(refl, -rep(i,sum(seminr_model$mmMatrix[,1] == seminr_model$ltVariables[i])))
    } else {
      refl <- c(refl, rep(i,sum(seminr_model$mmMatrix[,1] == seminr_model$ltVariables[i])))
    }
  }

  B <- t(seminr_model$path_coef)

  lambda <- seminr_model$outer_loadings[seminr_model$outer_loadings>0]

  exogenous <- setdiff(seminr_model$ltVariables,unique(seminr_model$smMatrix[,"target"]))
  sxixi <- cov(seminr_model$fscores[,exogenous],seminr_model$fscores[,exogenous])

  dat <- plssimul(B = B,lambda=lambda,w=NULL,sxx=NULL,syy=NULL,sxixi=sxixi,R2 = NULL,refl = refl,n = N,distr=distr)
  colnames(dat) <- seminr_model$mmVariables

  interactions = seminr_model$mobi_xm
  d <- dat
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

  #####
  power = function(theta, mu, se, n, alpha=0.05){
    crit.l = qt(alpha/2, mu, se)    ## Critical Value Based on Null
    crit.h = qt(1-alpha/2, mu, se)  ## Critical Value Based on Null
    pr.high = pt(crit.h, theta, sd = se,lower.tail=FALSE) ## Prob Reject High
    pr.low  = pt(crit.l, theta, sd = se)                  ## Prob Reject Low
    pow = pr.low+pr.high

    pow

  }
  power(thet=0.389, mu=0.5, se=0.089 , n = 100)
  power.t.test( 250 , bootstrapMatrix[1,2]-bootstrapMatrix[1,1] , 0.04905 , .05 , NULL , type = "two.sample" )
  power.t.test( 25 , -0.102 , 0.089 , .05 , NULL , type = "two.sample", strict = TRUE )

  ######

  ### All reflective ---------------------------------------------
  refl <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7)
  o <- order(refl)
  refl <- -refl

  B <- matrix(0, nrow = 7, ncol = 7)
  B[7,1:6] <- c(0.2,0.2,0.2,0.2,0.2,0.2)

  # vector of loadings
  lambda <- rep(0.9,21)

  # or vector of weights
  # w <- rep(0.4,21)

  # sxixi is cov matrix of reflective exogenous latents
  sxixi <- matrix(0.5,nrow = 6, ncol = 6)
  diag(sxixi) <- 1

  # set R2
  R2 <- 0.6

  ### All formative ---------------------------------------------

  refl <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7)
  #o <- order(refl)
  #refl <- -refl

  B <- matrix(0, nrow = 7, ncol = 7)
  B[6,1:5] <- c(0.2,0.2,0.2,0.2,0.2)
  B[7,1:5] <- c(0.2,0.2,0.2,0.2,0.2)

  # vector of loadings
  #lambda <- rep(0.9,21)

  # or vector of weights
  w <- rep(0.4,21)

  # sxx (p1,p1) covariance matrix of indicators of the exogeneous latent variables (formative relations)
  # syy (p2,p2) covariance matrix of indicators of the endogeneous latent variables (formative relations)
  sxx <- matrix(0.8,nrow = 15, ncol = 15)
  diag(sxx) <- 2.5

  syy <- matrix(0.8,nrow = 6, ncol = 6)
  diag(syy) <- 2.5

  # sxixi is cov matrix of reflective exogenous latents
  sxixi <- matrix(0.5,nrow = 6, ncol = 6)
  diag(sxixi) <- 1

  # set R2
  R2 <- 0.6

  ### Formative - Reflective  ---------------------------------------------

  refl <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, -6, -6, -6, -7, -7, -7)
  #o <- order(refl)
  #refl <- -refl

  B <- matrix(0, nrow = 7, ncol = 7)
  B[6,1:5] <- c(0.2,0.2,0.2,0.2,0.2)
  B[7,1:5] <- c(0.2,0.2,0.2,0.2,0.2)

  # vector of loadings
  lambda <- rep(0.9,6)

  # or vector of weights
  w <- rep(0.3,15)

  # sxx (p1,p1) covariance matrix of indicators of the exogeneous latent variables (formative relations)
  # syy (p2,p2) covariance matrix of indicators of the endogeneous latent variables (formative relations)
  sxx <- matrix(1.2,nrow = 15, ncol = 15)
  diag(sxx) <- 2.5

  syy <- matrix(0.8,nrow = 6, ncol = 6)
  diag(syy) <- 2.5

  # sxixi is cov matrix of reflective exogenous latents
  sxixi <- matrix(0.5,nrow = 6, ncol = 6)
  diag(sxixi) <- 1

  # set R2
  R2 <- 0.6





}
