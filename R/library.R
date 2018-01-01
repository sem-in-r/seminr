# function to get measurement mode of a latent (first item)
measure_mode <- function(latent,mmMatrix) {
  mmMatrix[mmMatrix[,"latent"]==latent,"type"][1]
}

# function to get measurement mode of a latent (first item) as a function
get_measure_mode <- function(latent,mmMatrix) {
  ifelse((mmMatrix[mmMatrix[,"latent"]==latent,"type"][1] == "A") |(mmMatrix[mmMatrix[,"latent"]==latent,"type"][1] == "C") , return(mode_A), return(mode_B))
#  base::get(mmMatrix[mmMatrix[,"latent"]==latent,"type"][1])
}

# Used in warnings - warning_only_causal_construct()
# function to get all the items of a given measurement mode for a given latent
items_per_mode <- function(latent, mode,mmMatrix) {
  latentmatrix <- mmMatrix[mmMatrix[,"latent"]==latent,c("measurement","type")]
  # If single item latent
  if (class(latentmatrix) == "character") {
    latentmatrix = t(as.matrix(latentmatrix))
  }
  return(latentmatrix[latentmatrix[,"type"] == mode,"measurement"])
}

# Used in warnings - warning_only_causal_construct() and warning_single_item_formative()
# function to subset and return the mmMatrix for a latent
mmMatrix_per_latent <- function(latent, mmMatrix) {
  latentmatrix <- mmMatrix[mmMatrix[,"latent"]==latent,c("latent","measurement","type")]
  # If single item latent
  if (class(latentmatrix) == "character") {
    latentmatrix = t(as.matrix(latentmatrix))
  }
  return(latentmatrix)
}

# Factorial weighting scheme Function to create inner paths matrix
#' @export
path_factorial <- function(smMatrix,fscores, dependant, paths_matrix) {
  inner_paths <- cor(fscores,fscores) * (paths_matrix + t(paths_matrix))
  return(inner_paths)
}

# Factorial weighting scheme Function to create inner paths matrix
#' @export
path_weighting <- function(smMatrix, fscores, dependant, paths_matrix) {
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

calculate_loadings <- function(weights_matrix,fscores, normData) {
  return(as.matrix(stats::cov(normData,fscores) * weights_matrix))
}

# Function to adjust for the interaction
# Adjustment of the SD of the interaction term as per Henseler, J., & Chin, W. W. (2010),
# A comparison of approaches for the analysis of interaction effects between latent variables
# using partial least squares path modeling. Structural Equation Modeling, 17(1), 82–109. https://doi.org/10.1080/10705510903439003
adjust_interaction <- function(ltVariables, mmMatrix, outer_loadings, fscores, obsData){
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


path_coef <- function(smMatrix, fscores,dependant, paths_matrix) {
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

standardize_outer_weights <- function(normData, mmVariables, outer_weights) {
  # Standardize the outer weights
  std_devs <- attr(scale((normData[,mmVariables]%*%outer_weights), center = FALSE),"scaled:scale")
  # divide matrix by std_devs and return
  return(t(t(outer_weights) / std_devs))
}

#' @export
mode_A  <- function(mmMatrix, i, normData, fscores) {
    return(stats::cov(normData[,mmMatrix[mmMatrix[,"latent"]==i,"measurement"]],fscores[,i]))
}

#' @export
correlation_weights <- mode_A

#' @export
mode_B <- function(mmMatrix, i,normData, fscores) {
    return(solve(stats::cor(normData[,mmMatrix[mmMatrix[,"latent"]==i,"measurement"]])) %*%
    stats::cor(normData[,mmMatrix[mmMatrix[,"latent"]==i,"measurement"]],
               fscores[,i]))
}

#' @export
regression_weights <- mode_B
