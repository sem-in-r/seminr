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
