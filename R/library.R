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

