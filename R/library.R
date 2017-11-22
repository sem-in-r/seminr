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

# This function is not used!!
# Function to estimate r_squared for endogenous constructs
estimate_Rsquared <- function(smMatrix,fscores) {
  #Calculate R Squared
  #Get smMatrix
  modelMatrix <- data.frame(smMatrix)
  #Get endogenous composites
  uniquetarget <- as.character(unique(modelMatrix$target))
  #Get composite scores
  valuesMatrix <- fscores
  #Calculate Linear Models
  lmmodels <- lapply(uniquetarget, function(x) {stats::lm(stats::as.formula(paste(x,"~ .", sep = "")),
                                                   data = data.frame(valuesMatrix[,colnames(valuesMatrix) %in%
                                                                                    c(x,as.character(modelMatrix$source[which(modelMatrix$target==x)]))]))})
  #Initialize matrix holder for Rsquared values
  rSquared <- matrix(,nrow=1,ncol=length(uniquetarget),byrow =TRUE,dimnames = list(1,uniquetarget))

  # Iterate and extract every R^2 value
  for (i in 1:length(lmmodels)) {
    rSquared[,i] <- summary(lmmodels[[i]])$r.squared
  }
  return(rSquared)
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
path.factorial <- function(smMatrix,fscores) {

  #Create a matrix of inner paths
  #? inner_paths => inner_weights?
  inner_paths <- matrix(data=0,
                        nrow=length(unique(c(smMatrix[,1],smMatrix[,2]))),
                        ncol=length(unique(c(smMatrix[,1],smMatrix[,2]))),
                        dimnames = list(unique(c(smMatrix[,1],smMatrix[,2])),unique(c(smMatrix[,1],smMatrix[,2]))))

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
path.weighting <- function(smMatrix, fscores) {

  #Create list of Latent Variables
  ltVariables <- unique(c(smMatrix[,1],smMatrix[,2]))

  #Identify Endogenous Variables
  dependant <- unique(smMatrix[,2])

  #Create a matrix of inner paths
  #? inner_paths => inner_weights?
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
    inner_paths[rownames(results),dependant[i]] = results
    }
  return(inner_paths)
}

calculate.loadings <- function(mmMatrix, ltVariables,fscores, normData) {
  #Create a matrix of Outer Loadings
  outer_loadings <- matrix(data=0,
                           nrow=nrow(mmMatrix),
                           ncol=length(ltVariables),
                           dimnames = list(mmMatrix[,2],ltVariables))


  #Calculate the Outer Loadings
  for (i in 1:length(ltVariables))  {
    outer_loadings [mmMatrix[mmMatrix[,"latent"]==ltVariables[i],
                             "measurement"],
                    ltVariables[i]] = stats::cov(normData[,mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"measurement"]],fscores[,ltVariables[i]])

  }
  return(outer_loadings)
}
