# function to get measurement mode of a latent (first item)
measure_mode <- function(latent,mmMatrix) {
  mmMatrix[mmMatrix[,"latent"]==latent,"type"][1]
}

# TODO: Remove if not used
# function to get all the items of a given measurement mode for a given latent
items_per_mode <- function(latent, mode,mmMatrix) {
  latentmatrix <- mmMatrix[mmMatrix[,"latent"]==latent,c("measurement","type")]
#  if(class(latentmatrix) == "matrix") {
#  }
  if (class(latentmatrix) == "character") {
    latentmatrix = t(as.matrix(latentmatrix))
  }
  return(latentmatrix[latentmatrix[,"type"] == mode,"measurement"])
}

# TODO: Remove if not used
# function to subset and return the mmMatrix for a latent
mmMatrix_per_latent <- function(latent, mmMatrix) {
  latentmatrix <- mmMatrix[mmMatrix[,"latent"]==latent,c("latent","measurement","type")]
  if(class(latentmatrix) == "matrix") {
    return(latentmatrix)
  }
  if (class(latentmatrix) == "character") {
    latentmatrix = t(as.matrix(latentmatrix))
    return(latentmatrix)
  }
}

# Function to create a named vector of path coefficients
# TODO: check whether all these conditions occur and whether we should improve or document
transform_to_named_vector <- function(results,independant) {
  coefficients <- as.vector(results)
  if(!is.null(rownames(results))) {
    names(coefficients)<-rownames(results)
  } else if (!is.null(names(results))) {
    names(coefficients)<-names(results)
  } else {
    names(coefficients)<-independant
  }
  return(coefficients)
}

# Factorial weighting scheme Function to create inner paths matrix
#' @export
path.factorial <- function(smMatrix,fscores, dependant, paths_matrix) {
  inner_paths <- cor(fscores,fscores) * (paths_matrix + t(paths_matrix))
  return(inner_paths)
}

# Factorial weighting scheme Function to create inner paths matrix
#' @export
path.weighting <- function(smMatrix, fscores, dependant, paths_matrix) {
  # correlations for outgoing paths
  inner_paths <- cor(fscores,fscores) * t(paths_matrix)

  #Regression betas for the incoming paths
  #Iterate and regress the incoming paths
  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]

    #Solve the system of equations
    inner_paths[independant,dependant[i]] = solve(t(fscores[,independant]) %*% fscores[,independant], t(fscores[,independant]) %*% fscores[,dependant[i]])
  }
  return(inner_paths)
}

calculate.loadings <- function(weights_matrix,fscores, normData) {
  return(as.matrix(stats::cov(normData,fscores) * weights_matrix))
}

# Function to adjust for the interaction
# TODO: add a citation in the comments here replace this line
adjust.interaction <- function(ltVariables, mmMatrix, outer_loadings, fscores, obsData){
  for(latent in ltVariables) {
    adjustment <- 0
    denom <- 0
    if(grepl("\\.", latent)) {
      list <- mmMatrix[mmMatrix[,"latent"]==latent,"measurement"]

      for (item in list){
        adjustment <- adjustment + stats::sd(obsData[,item])*abs(as.numeric(outer_loadings[item,latent]))
        denom <- denom + abs(outer_loadings[item,latent])
      }
      adjustment <- adjustment/denom
      fscores[,latent] <- fscores[,latent]*adjustment
    }
  }
  return(fscores)

}


path.coef <- function(smMatrix, fscores,dependant, paths_matrix) {
  #Regression betas for the incoming paths
  #Iterate and regress the incoming paths
  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]

    #Solve the system of equations
    paths_matrix[independant,dependant[i]] = solve(t(fscores[,independant]) %*% fscores[,independant], t(fscores[,independant]) %*% fscores[,dependant[i]])
  }
  return(paths_matrix)
}



### This metric should be moved to a metrics folder
# BIC function using rsq, SST, n pk
BIC_func <- function(rsq, pk, N, fscore){
  SSerrk <- (1-rsq)*(stats::var(fscore)*(N-1))
  N*(log(SSerrk/N)) + (pk+1)*log(N)
}

# calculating insample metrics
calc.insample <- function(obsData, fscores, smMatrix, dependant) {
  insample <- matrix(,nrow=3,ncol=length(dependant),byrow =TRUE,dimnames = list(c("Rsq","AdjRsq","BIC"),dependant))

  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]

    #Calculate insample
    for (j in 1:length(independant)) {

      # Calculate r-squared for the endogenous variable
      fscore_cors <- stats::cor(fscores)
      r_sq <- 1 - 1/solve(fscore_cors[c(independant,dependant[i]),c(independant,dependant[i])])
      insample[1,i] <- r_sq[dependant[i],dependant[i]]
      insample[2,i] <- 1 - (1 - insample[1,i])*((nrow(obsData)-1)/(nrow(obsData)-length(independant) - 1))
      # Calculate the BIC for the endogenous
      insample[3,i] <- BIC_func(r_sq[dependant[i],dependant[i]],length(independant),nrow(obsData),fscores[,dependant[i]])

    }
  }
  return(insample)
}

standardize.outer.weights <- function(normData, mmVariables, outer_weights) {
  # Standardize the outer weights
  std_devs <- attr(scale((normData[,mmVariables]%*%outer_weights), center = FALSE),"scaled:scale")
  # divide by matrix bvy std_devs and return
  return(t(t(outer_weights) / std_devs))
}
