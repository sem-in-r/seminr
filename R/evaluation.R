evaluate_model <- function(seminr_model) {
  rel <- reliability(seminr_model)
  val <- validity(seminr_model)
  out <- list(rel,val)
  names(out) <- c("Reliability","Validity")
  return(out)
}


## Reliability -------------------------
# RhoC and AVE
# Dillon-Goldstein's Rho as per: Dillon, W. R, and M. Goldstein. 1987. „Multivariate Analysis: Methods
# and Applications.“ Biometrical Journal 29 (6): 750–756.
# Average Variance Extracted as per:  Fornell, C. and D. F. Larcker (February 1981). Evaluating
# structural equation models with unobservable variables and measurement error, Journal of Marketing Research, 18, pp. 39-5
rhoC_AVE <- function(seminr_model){
  dgr <- matrix(NA, nrow=length(seminr_model$ltVariables), ncol=2)
  rownames(dgr) <- seminr_model$ltVariables
  colnames(dgr) <- c("rhoC", "AVE")
  for(i in seminr_model$ltVariables){
    x <- seminr_model$outer_loadings[, i]
    if(measure_mode(i,seminr_model$mmMatrix)=="B"| measure_mode(i,seminr_model$mmMatrix)=="A"){
      ind <- which(x!=0)
      if(length(ind)==1){
        dgr[i,1:2] <- 1
      } else {
       x <- x[ind]
       dgr[i,1] <- sum(x)^2 / (sum(x)^2 + sum(1-x^2))
       dgr[i,2] <- sum(x^2)/length(x)
      }
    } else {
      dgr[i,1] <- NA
      dgr[i,2] <- sum(x^2)/length(x)
    }
  }
  return(dgr)
}

# rhoA as per Dijkstra, T. K., & Henseler, J. (2015). Consistent Partial Least Squares Path Modeling, 39(X).
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

validity <- function(seminr_model) {
  cl <- cross_loadings(seminr_model)
# Remove HTMT
#  htmt <- HTMT(seminr_model)
#  out <- list(cl,htmt)
  out <- list(cl)
#  names(out) <- c("Cross-Loadings", "HTMT")
  names(out) <- "Cross-Loadings"
  return(out)
}

cross_loadings <- function(seminr_model) {
  return(stats::cor(seminr_model$data[,seminr_model$mmVariables],seminr_model$fscores))
}

# HTMT as per Henseler, J., Ringle, C. M., & Sarstedt, M. (2014). A new criterion for assessing discriminant validity in
# variance-based structural equation modeling. Journal of the Academy of Marketing Science, 43(1), 115–135.
# https://doi.org/10.1007/s11747-014-0403-8
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

## Construct FIT ---------------------

# BIC function using rsq, SST, n pk
BIC_func <- function(rsq, pk, N, fscore){
  SSerrk <- (1-rsq)*(stats::var(fscore)*(N-1))
  N*log(SSerrk/N) + (pk+1)*log(N)
}

# AIC function using rsq, SST, n pk
AIC_func <- function(rsq, pk, N, fscore){
  SSerrk <- (1-rsq)*(stats::var(fscore)*(N-1))
  2*(pk+1)+N*log(SSerrk/N)
}

# calculating insample metrics
calc_insample <- function(obsData, fscores, smMatrix, dependant, fscore_cors) {
  # matrix includes BIC
  # Remove BIC for now
  #insample <- matrix(,nrow=3,ncol=length(dependant),byrow =TRUE,dimnames = list(c("Rsq","AdjRsq","BIC"),dependant))

  # matrix excludes BIC
  insample <- matrix(,nrow=2,ncol=length(dependant),byrow =TRUE,dimnames = list(c("Rsq","AdjRsq"),dependant))

  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]

    #Calculate insample for endogenous
    #    fscore_cors <- stats::cor(fscores)
    r_sq <- 1 - 1/solve(fscore_cors[c(independant,dependant[i]),c(independant,dependant[i])])
    insample[1,i] <- r_sq[dependant[i],dependant[i]]
    insample[2,i] <- 1 - (1 - insample[1,i])*((nrow(obsData)-1)/(nrow(obsData)-length(independant) - 1))
    # Calculate the BIC for the endogenous
    # Remove BIC for now
    #insample[3,i] <- BIC_func(r_sq[dependant[i],dependant[i]],length(independant),nrow(obsData),fscores[,dependant[i]])
  }
  return(insample)
}
