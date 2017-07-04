
rhoA <- function(plsModel) {
  # get latent variable scores and weights for each latent
  latentscores <- plsModel$fscores
  weights <- plsModel$outer_weights
  # get the mmMatrix and smMatrix
  mmMatrix <- plsModel$mmMatrix
  smMatrix <- plsModel$smMatrix
  # Create rhoA holder matrix
  rhoA <- matrix(,nrow = nrow(oldcor),ncol = 1,dimnames = list(rownames(oldcor),c("rhoA")))
  
  for (i in rownames(rhoA))  {
    #If the measurement model is Formative assign rhoA = 1
    if(mmMatrix[mmMatrix[,"latent"]==i,"type"][1]=="F"){
      rhoA[i,1] <- 1
    }
    #If the measurement model is Reflective Calculate RhoA
    if(mmMatrix[mmMatrix[,"latent"]==i,"type"][1]=="R"){
      # get the weights for the latent
      w <- as.matrix(weights[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i])

      # Get empirical covariance matrix of lv indicators (S)
      indicators <- scale(plsModel$obsData[,mmMatrix[mmMatrix[,"latent"]==i,"measurement"]],TRUE,TRUE)
      S <- cov(indicators,indicators)
      diag(S) <- 0
      
      # Get AA matrix without diagonal 
      AAnondiag <- w %*% t(w)
      diag(AAnondiag) <- 0
      
      # Calculate rhoA
      rhoA[i,1] <- (t(w) %*% w)^2 * ((t(w) %*% (S) %*% w)/(t(w) %*% AAnondiag %*% w))
    }
  }
return(rhoA)
}

# End rhoA function


# Calculate PLSc function
#1 Determine inconsistent lv correlations from simplePLS
latentscores <- plsModel$fscores
oldcor <- cor(latentscores,latentscores)
oldcor[lower.tri(oldcor)] <- 0
diag(oldcor) <- 0

oldcor[1,2] <- oldcor[1,2]*

rhoA <- 
