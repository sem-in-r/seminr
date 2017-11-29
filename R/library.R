# function to get measurement mode of a latent (first item)
measure_mode <- function(latent,mmMatrix) {
  mmMatrix[mmMatrix[,"latent"]==latent,"type"][1]
}

# function to get all the items of a given measurement mode for a given latent
items_per_mode <- function(latent, mode,mmMatrix) {
  latentmatrix <- mmMatrix[mmMatrix[,"latent"]==latent,c("measurement","type")]
  if(class(latentmatrix) == "matrix") {
    return(latentmatrix[latentmatrix[,"type"] == mode,"measurement"])
  }
  if (class(latentmatrix) == "character") {
    latentmatrix = t(as.matrix(latentmatrix))
    return(latentmatrix[latentmatrix[,"type"] == mode,"measurement"])
  }
}

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
path.factorial <- function(smMatrix,fscores, dependant, ltVariables) {

  #Create a matrix of inner paths
  inner_paths <- matrix(data=0,
                        nrow=length(ltVariables),
                        ncol=length(ltVariables),
                        dimnames = list(ltVariables,ltVariables))

  #Estimate inner paths (symmetric matrix)
  for (i in 1:nrow(smMatrix))  {
    inner_paths[smMatrix[i,"source"],
                smMatrix[i,"target"]] = stats::cor(fscores[,smMatrix[i,"source"]],
                                                   fscores[,smMatrix[i,"target"]])
    #? next step necessary?
    inner_paths[smMatrix[i,"target"],
                smMatrix[i,"source"]] = stats::cor(fscores[,smMatrix[i,"source"]],
                                                   fscores[,smMatrix[i,"target"]])
  }
  return(inner_paths)
}

# Factorial weighting scheme Function to create inner paths matrix
path.weighting <- function(smMatrix, fscores, dependant, ltVariables) {

  #Create a matrix of inner paths
  inner_paths <- matrix(data=0,
                        nrow=length(ltVariables),
                        ncol=length(ltVariables),
                        dimnames = list(ltVariables,ltVariables))

  #Estimate inner paths (a-symmetric matrix)
  #Correlations for outgoing paths
  for (i in 1:nrow(smMatrix))  {
    inner_paths[smMatrix[i,"target"],
                smMatrix[i,"source"]] = stats::cor(fscores[,smMatrix[i,"source"]],
                                                   fscores[,smMatrix[i,"target"]])
  }

  #Regression betas for the incoming paths
  #Iterate and regress the incoming paths
  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]

    #Solve the system of equations
    results = solve(t(fscores[,independant]) %*% fscores[,independant]) %*% (t(fscores[,independant]) %*% fscores[,dependant[i]])

    #solve the system of equations and Assign the inner weights to the Matrix
    inner_paths[independant,dependant[i]] = results
  }
  return(inner_paths)
}

calculate.loadings <- function(weights_matrix,fscores, normData) {
  cov(normData,fscores) * weights_matrix
}

# Function to adjust for the interaction
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


path.coef <- function(smMatrix, fscores,dependant,ltVariables) {

  #Create a matrix of inner paths
  #? inner_paths => inner_weights?
  inner_paths <- matrix(data=0,
                        nrow=length(ltVariables),
                        ncol=length(ltVariables),
                        dimnames = list(ltVariables,ltVariables))

  #Regression betas for the incoming paths
  #Iterate and regress the incoming paths
  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]

    #Solve the system of equations
    results = solve(t(fscores[,independant]) %*% fscores[,independant]) %*% (t(fscores[,independant]) %*% fscores[,dependant[i]])

    #solve the system of equations and Assign the inner weights to the Matrix
    inner_paths[independant,dependant[i]] = results
  }
  return(inner_paths)
}

### This metric should be moved to a metrics folder

calc.rSquared <- function(obsData, fscores, smMatrix, dependant) {
  rSquared <- matrix(,nrow=2,ncol=length(dependant),byrow =TRUE,dimnames = list(c("Rsq","AdjRsq"),dependant))

  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]

    #Calculate Rsquared
    for (j in 1:length(independant)) {

      # Calculate r-squared for the endogenous variable
      fscore_cors <- stats::cor(fscores)
      r_sq <- 1 - 1/solve(fscore_cors[c(independant,dependant[i]),c(independant,dependant[i])])
      rSquared[1,i] <- r_sq[dependant[i],dependant[i]]
      rSquared[2,i] <- 1 - (1 - rSquared[1,i])*((nrow(obsData)-1)/(nrow(obsData)-length(independant) - 1))
    }
  }
  return(rSquared)
}

standardize.outer.weights <- function(normData, mmVariables, outer_weights) {
  # Standardize the outer weights
  std_devs <- attr(scale((normData[,mmVariables]%*%outer_weights), center = FALSE),"scaled:scale")
  # divide by matrix bvy std_devs and return
  return(t(t(outer_weights) / std_devs))
}
