## Implement SRMR
# This is not working -------------------
SRMR <- function(seminr_model) {
  obs <- mobi_pls$path_coef
  diag(obs) <- 1

  imp <- stats::cor(mobi_pls$fscores)
  lobs <-  obs[!lower.tri(obs)]
  limp <-  imp[!lower.tri(imp)]
  limp[lobs==0] <- 0

  sqrt(mean((limp - lobs)^2))
}
#  End of not working -------------------

# RhoC and AVE



rhoC_AVE <- function(seminr_model){
  dgr <- matrix(NA, nrow=length(seminr_model$ltVariables), ncol=2)
  rownames(dgr) <- seminr_model$ltVariables
  colnames(dgr) <- c("rhoC", "AVE")
  for(i in seminr_model$ltVariables){
    if(measure_mode(i,seminr_model$mmMatrix)=="B"| measure_mode(i,seminr_model$mmMatrix)=="A"){
      x <- seminr_model$outer_loadings[, i]
      ind <- which(x!=0)
      if(length(ind)==1){
        dgr[i,1:2] <- 1
      } else {
       x <- x[ind]
       dgr[i,1] <- sum(x)^2 / (sum(x)^2 + sum(1-x^2))
       dgr[i,2] <- sum(x^2)/length(x)
      }
    } else {
      dgr[i,1] <- N/A
      dgr[i,2] <- sum(x^2)/length(x)
    }
  }
  return(dgr)
}

rho_A <- function(seminr_model) {
  # get latent variable scores and weights for each latent
  latentscores <- seminr_model$fscores
  weights <- seminr_model$outer_weights
  # get the mmMatrix and smMatrix
  mmMatrix <- seminr_model$mmMatrix
  smMatrix <- seminr_model$smMatrix
  obsData <- seminr_model$data
  # Create rhoA holder matrix
  rho <- matrix(,nrow = ncol(latentscores),ncol = 1,dimnames = list(colnames(latentscores),c("rhoA")))

  for (i in rownames(rho))  {
    #If the measurement model is Formative assign rhoA = 1
    if(mmMatrix[mmMatrix[,"latent"]==i,"type"][1]=="B"){
      rho[i,1] <- 1
    }
    #If the measurement model is Reflective Calculate RhoA
    if(mmMatrix[mmMatrix[,"latent"]==i,"type"][1]=="C" | mmMatrix[mmMatrix[,"latent"]==i,"type"][1]=="A"){
      #if the latent is a single item rhoA = 1
      if(nrow(mmMatrix_per_latent(i,mmMatrix)) == 1) {
        rho[i,1] <- 1
      } else {
        # get the weights for the latent
        w <- as.matrix(weights[mmMatrix[mmMatrix[,"latent"]==i,"measurement"],i])

        # Get empirical covariance matrix of lv indicators (S)
        indicators <- scale(obsData[,mmMatrix[mmMatrix[,"latent"]==i,"measurement"]],TRUE,TRUE)
        S <- stats::cov(indicators,indicators)
        diag(S) <- 0

        # Get AA matrix without diagonal
        AAnondiag <- w %*% t(w)
        diag(AAnondiag) <- 0

        # Calculate rhoA
        rho[i,1] <- (t(w) %*% w)^2 * ((t(w) %*% (S) %*% w)/(t(w) %*% AAnondiag %*% w))
      }
    }
  }
  return(rho)
}
reliability <- function(seminr_model) {
  mat1 <- rhoC_AVE(seminr_model)
  mat2 <- rho_A(seminr_model)
  return(cbind(mat1,mat2))
}

## Validity ---------------------

cross_loadings <- function(seminr_model) {
  return(stats::cor(seminr_model$data[,seminr_model$mmVariables],seminr_model$fscores))
}

HTMT <- function(seminr_model) {
  HTMT <- matrix(, nrow=length(seminr_model$ltVariables), ncol=length(seminr_model$ltVariables),
                 dimnames = list(seminr_model$ltVariables,seminr_model$ltVariables))
  for (latenti in seminr_model$ltVariables[1:(length(seminr_model$ltVariables)-1)]) {
    for (latentj in seminr_model$ltVariables[(which(seminr_model$ltVariables == latenti)+1):length(seminr_model$ltVariables)]) {
      manifesti <- seminr_model$mmVariables[seminr_model$mmMatrix[,1] == latenti]
      manifestj <- seminr_model$mmVariables[seminr_model$mmMatrix[,1] == latentj]
      item_correlation_matrix <- stats::cor(seminr_model$data[,manifesti],seminr_model$data[,manifestj])
      HTHM <- mean(item_correlation_matrix)
      if(length(manifesti)>1 ) {
        cor_matrix <- stats::cor(seminr_model$data[,manifesti],seminr_model$data[,manifesti])
        diag(cor_matrix) <- 0
        MTHM <- (2/(length(manifesti)*(length(manifesti)-1)))*(sum(cor_matrix[!lower.tri(cor_matrix)]))
      } else {
        MTHM <- 1
      }
      if(length(manifestj)>1) {
        cor_matrix2 <- stats::cor(seminr_model$data[,manifestj],seminr_model$data[,manifestj])
        diag(cor_matrix2) <- 0
        MTHM <- sqrt(MTHM * (2/(length(manifestj)*(length(manifestj)-1)))*(sum(cor_matrix2[!lower.tri(cor_matrix2)])))
      } else {
        MTHM <- sqrt(1 * MTHM)
      }
      HTMT[latenti,latentj] <- HTHM / MTHM
    }
  }
  return(HTMT)
}

