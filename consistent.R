
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

PLSc <- function(plsModel) {
  # Calculate PLSc function
  # get smMatrix and dependant latents, rhoA
  smMatrix <- plsModel$smMatrix
  dependant <- unique(smMatrix[,"target"])
  rhoA <- rhoA(plsModel)
  latents <- unique(as.vector(unique(smMatrix)))
  path_coef <- plsModel$path_coef
  # Determine inconsistent lv correlations from simplePLS
  latentscores <- plsModel$fscores
  oldcor <- cor(latentscores,latentscores)
  
  # adjust all the latents
  for(i in latents) {
    for(j in latents) {
      if(i == j) {
        oldcor[i,j] <- 1
      }
      if (i != j) {
        oldcor[i,j] <- oldcor[i,j]/(sqrt(rhoA[i,1]*rhoA[j,1]))
      }
    }
  }

  for (i in dependant)  {
    
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant,"source"]
    
    #Solve the sistem of equations
    results<- solve(oldcor[independant,independant],
                    oldcor[independant,dependant])
    
    #Transform to a generic vector
    coefficients <- as.vector(results)
    if(!is.null(rownames(results)))
      names(coefficients)<-rownames(results)
    else
      names(coefficients)<-names(results)
    
    #Assign the Beta Values to the Path Coefficient Matrix
    for (j in 1:length(independant))  
      path_coef[independant[j],dependant]=coefficients[independant[j]]
    
  }
  plsModel$path_coef <- path_coef
  return(plsModel)
}
# end PLSc function
