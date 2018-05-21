evaluate_model <- function(seminr_model) {
  rel <- reliability(seminr_model)
  val <- validity(seminr_model)
  out <- list(rel,val)
  names(out) <- c("reliability","validity")
  return(out)
}


## Reliability -------------------------
reliability <- function(seminr_model) {
  mat1 <- rhoC_AVE(seminr_model)
  mat2 <- rho_A(seminr_model)
  return(cbind(mat1,mat2))
}

## Validity ---------------------
validity <- function(seminr_model) {
  list(
    # htmt            = HTMT(seminr_model),
    cross_loadings  = cross_loadings(seminr_model),
    item_vifs       = item_vifs(seminr_model),
    antecedent_vifs = antecedent_vifs(seminr_model)
  )
}

cross_loadings <- function(seminr_model) {
  return(stats::cor(seminr_model$data[,seminr_model$mmVariables],seminr_model$construct_scores))
}

# HTMT as per Henseler, J., Ringle, C. M., & Sarstedt, M. (2014). A new criterion for assessing discriminant validity in
# variance-based structural equation modeling. Journal of the Academy of Marketing Science, 43(1), 115-135.
# https://doi.org/10.1007/s11747-014-0403-8
HTMT <- function(seminr_model) {
  HTMT <- matrix(, nrow=length(seminr_model$constructs), ncol=length(seminr_model$constructs),
                 dimnames = list(seminr_model$constructs,seminr_model$constructs))
  for (constructi in seminr_model$constructs[1:(length(seminr_model$constructs)-1)]) {
    for (constructj in seminr_model$constructs[(which(seminr_model$constructs == constructi)+1):length(seminr_model$constructs)]) {
      manifesti <- seminr_model$mmVariables[seminr_model$mmMatrix[,1] == constructi]
      manifestj <- seminr_model$mmVariables[seminr_model$mmMatrix[,1] == constructj]
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
      HTMT[constructi,constructj] <- HTHM / MTHM
    }
  }
  return(HTMT)
}

## Construct FIT ---------------------

# BIC function using rsq, SST, n pk
BIC_func <- function(rsq, pk, N, construct_score){
  SSerrk <- (1-rsq)*(stats::var(construct_score)*(N-1))
  N*log(SSerrk/N) + (pk+1)*log(N)
}

# AIC function using rsq, SST, n pk
AIC_func <- function(rsq, pk, N, construct_score){
  SSerrk <- (1-rsq)*(stats::var(construct_score)*(N-1))
  2*(pk+1)+N*log(SSerrk/N)
}

# calculating insample metrics
calc_insample <- function(obsData, construct_scores, smMatrix, dependant, construct_score_cors) {
  # matrix includes BIC
  # Remove BIC for now
  #insample <- matrix(,nrow=3,ncol=length(dependant),byrow =TRUE,dimnames = list(c("Rsq","AdjRsq","BIC"),dependant))

  # matrix excludes BIC
  insample <- matrix(,nrow=2,ncol=length(dependant),byrow =TRUE,dimnames = list(c("Rsq","AdjRsq"),dependant))

  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]

    #Calculate insample for endogenous
    #    construct_score_cors <- stats::cor(construct_scores)
    r_sq <- 1 - 1/solve(construct_score_cors[c(independant,dependant[i]),c(independant,dependant[i])])
    insample[1,i] <- r_sq[dependant[i],dependant[i]]
    insample[2,i] <- 1 - (1 - insample[1,i])*((nrow(obsData)-1)/(nrow(obsData)-length(independant) - 1))
    # Calculate the BIC for the endogenous
    # Remove BIC for now
    #insample[3,i] <- BIC_func(r_sq[dependant[i],dependant[i]],length(independant),nrow(obsData),construct_scores[,dependant[i]])
  }
  return(insample)
}
