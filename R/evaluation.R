evaluate_model <- function(seminr_model) {
  gof <- GoF(seminr_model)
  rel <- reliability(seminr_model)
  val <- validity(seminr_model)
  out <- list(gof,rel,val)
  names(out) <- c("Goodness-of-Fit","Reliability","Validity")
  return(out)
}


## Goodness-of-fit -----------------------
## SRMR
SRMR <- function(seminr_model) {
  # calculate the observed correlation matrix
  obs <- cor(seminr_model$data[,seminr_model$mmVariables],seminr_model$data[,seminr_model$mmVariables])
  # Calculate the implied correlation matrix
  imp <- seminr_model$outer_loadings %*% t(seminr_model$outer_loadings)
  # diagonals will be excluded from differences
  diag(imp) <- 0
  diag(obs) <- 0

  # just take upper triangle to avoid duplication
  lobs <-  obs[!lower.tri(obs)]
  limp <-  imp[!lower.tri(imp)]
  # Remove correlations in observed that do not occur in implied
  lobs[limp==0] <- 0

  # retain vector of only non-zero correlations
  lobs <- lobs[lobs>0]
  limp <- limp[limp>0]
  #calculate SRMR
  return(as.numeric(sqrt(mean((limp - lobs)^2))))
}

GoF <- function(seminr_model) {
  SRMR(seminr_model)
}

## Reliability -------------------------
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

validity <- function(seminr_model) {
  cl <- cross_loadings(seminr_model)
  htmt <- HTMT(seminr_model)
  out <- list(cl,htmt)
  names(out) <- c("Cross-Loadings", "HTMT")
  return(out)
}

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

fitted.matrixpls <- function(object, ...) {

  # Equation numbers in parenthesis refer to equation number in Lohmoller 1989

  nativeModel <- attr(object,"model")

  # Check that the matrices form a valid model

  if(any(rowSums(nativeModel$formative) > 0 & rowSums(nativeModel$formative) > 0))
    stop("Cannot calculate model implied covariance matrix. A composite is a dependent variable in both formative and inner matrices resulting in an impossible model")

  S <- cov(seminr_model$data[,seminr_model$mmVariables])
  IC <- cov(seminr_model$fscores,seminr_model$data[,seminr_model$mmVariables])
  C <- cor(seminr_model$fscores)
  B <- t(seminr_model$path_coef) ####(maybe t()??????) B = Inner
  F <- matrix(0,nrow = 4,ncol = 13)
  L <- seminr_model$outer_weights[seminr_model$mmMatrix[,4] == "A",]

  r2 <- rowSums(B * C)

  # Matrices containing all regressions and covariances
  # indicators first, then composites

  fullB <- rbind(cbind(matrix(0,nrow(S),nrow(S)),L),
                 cbind(F,B))

  fullC <- rbind(cbind(S,t(IC)),
                 cbind(IC,C))

  exog <- rowSums(fullB) == 0

  # Add indicator errors and composite errors

  e <- c(diag(S) - rowSums(L * t(IC)),
         1-r2)

  fullC <- rbind(cbind(fullC,matrix(0, nrow(fullC), sum(!exog))),
                 cbind(matrix(0, sum(!exog), nrow(fullC)), diag(e)[!exog,!exog]))

  fullB <- cbind(fullB,diag(length(e))[,! exog])
  fullB <- rbind(fullB, matrix(0,sum(!exog), ncol(fullB)))

  # Update exog because fullB is now larger
  exog <- rowSums(fullB) == 0

  #
  # Derive the implied covariance matrix using the Bentler-Weeks model
  #
  # Bentler, P. M., & Weeks, D. G. (1980). Linear structural equations with latent variables. Psychometrika, 45(3), 289–308. doi:10.1007/BetaF02293905
  #

  # beta (regression paths between dependent variables)

  beta <- fullB[!exog,!exog]


  # gamma (regression paths between dependent and independent  variables)

  gamma <- fullB[!exog,exog]

  # exogenous variable covariance matrix
  Phi <- fullC[exog,exog]

  #
  # Calculate sigma
  #

  # Matrix orders

  # number of observed dependent
  p = sum(!exog[1:nrow(S)])
  # number of observed independent
  q = nrow(S)-p

  # number of independent variables
  n = nrow(Phi)
  # number of dependent variables
  m = nrow(beta)

  # Number of observed variables
  r = p + q
  # Number of variables
  s = m + n

  # The matrices

  Beta <- matrix(0,s,s)
  Gamma <- matrix(0,s,n)
  G <- matrix(0,r,s)

  # Identity matrix

  I<-diag(nrow(Beta))

  # Populate the parameter matrices

  Gamma[1:m,] <- gamma
  Gamma[(m+1):s,] <- diag(n)

  Beta[1:m,1:m]<-beta

  # Populate the selection matrix (columns of all variables  selected to rows of observed variables)
  observed <- NULL
  if(p>0){
    observed <- 1:p
  }
  if(q>0){
    observed <- c(observed,(1:q)+n)
  }

  G <- diag(s)[observed,]

  # G has dependent variables followed by independent variables. Reorder to match the variables
  # in S

  G <- G[order(exog[1:nrow(S)]),]

  # Calculate the model implied covariance matrix and return

  Sigma = G %*% solve(I-Beta) %*% Gamma %*% Phi %*% t(Gamma) %*% t(solve(I-Beta))%*% t(G)
  rownames(Sigma) <- colnames(Sigma) <- rownames(S)

  Sigma
}


residuals.matrixpls <- function(object, ..., observed = TRUE) {

  RMS <- function(num) sqrt(sum(num^2)/length(num))

  S <- cov(seminr_model$data[,seminr_model$mmVariables])

  # Lohmöller defines quite a few statistics based on correlations.
  # Because S is a covariance matrix, we need to calculate the
  # corresponding correlation matrix as well.

  Scor <- stats::cov2cor(S)

  nativeModel <- attr(object,"model")

  # Equation numbers in parenthesis refer to equation number in Lohmoller 1989

  if(observed){

    W <- t(seminr_model$outer_weights)

    # Number of reflective indicators
    reflectiveIndicators<- rowSums(seminr_model$outer_loadings)>0
    k <- sum(reflectiveIndicators)

    # Number of endog LVs
    endog <- rowSums(t(seminr_model$path_coef))>0
    h <- sum(endog)



    # Factor loading matrix
    P <- seminr_model$outer_loadings

    P[P==1] <- object[grepl("=~", names(object), fixed=TRUE)]

    # Standardized loadings
    Pstd <- sweep(P,MARGIN=1,sqrt(diag(S)),`/`)

    # This is always standardized, so no need to rescale
    B <- t(seminr_model$path_coef)

    # Lohmoller is not clear whether R should be based on the estimated betas or calculated scores
    # The scores are used here because this results in less complex code

    R <- cor(seminr_model$fscores)
    R_star <- (B %*% R %*% t(B))[endog,endog] # e. 2.99


    # Model implied indicator correlations
    H <- Pstd %*% R %*% t(Pstd) # eq 2.96

    I <- diag(ncol(Scor))
    H2 <- (I * H)  %*% solve(I * Scor) # eq 2.97

    F <- Pstd %*% B %*% R %*% t(B) %*% t(Pstd) # eq 2.104

    F2 <- (I * F) %*% solve(I * Scor) # eq 2.105

    r2 <- rowSums(B * cor(seminr_model$fscores))


    # Lohmoller 1989 uses C for the residual covariance matrix of indicators
    # matrixpls uses C for the composite correlation matrix, but from here on
    # until the end of the function, C is used for the residual covariance matrix

    # Lohmoller does not define C in covariance form, so we need to do it ourselfs.
    #
    # Start with the observed residuals:
    #
    # e = X-XW'P'
    #
    # because residuals have a mean of zero cov(e) can be defined as
    #
    # cov(e) = (X-XW'P')’(X-XW'P')
    # cov(e) = (X’-(XW'P')')(X-XW'P')
    # cov(e) = (X’-PWX')(X-XW'P')
    # cov(e) = X’X-X’XW'P'-PWX'X+PWX'XW'P'
    # cov(e) = S-SW'P'-PWS+PWSXW'P'
    # cov(e) = S-SW'P'-(SW'P')’+PRP'

    C <- S - S%*%t(W)%*%t(P) - t(S-S%*%t(W)%*%t(P)) + P%*%R%*%t(P)

    Q <- (W %*% S %*% t(W))[endog,endog] - R_star


    indices <- c(Communality = psych::tr(H2)/k, # eq 2.109
                 Redundancy = psych::tr(F2)/k,  # eq 2.110
                 SMC = sum(r2)/h,         # eq 2.111
                 "RMS outer residual covariance" = RMS(C[lower.tri(C)]), # eq 2.118
                 "RMS inner residual covariance" = RMS(C[lower.tri(Q)]) # eq 2.118
    )
  }
  else{
    #C <- S-stats::fitted(object)
    C <- S-Sigma
    indices <- c()
  }

  C_std <- diag(1/diag(S)) %*% C

  indices <- c(indices,c(

    # SRMR as calculated in SEM. (Hu and Bentler, 1999, p. 3)

    SRMR = sqrt(sum(C_std[lower.tri(C_std)]^2)/length(C[lower.tri(C_std, diag=TRUE)])),

    # SRMR calculated ignoring within block residuals from Henserler et al 2014.

    "SRMR (Henseler)" = sqrt(2*sum((C_std[(seminr_model$outer_loadings %*% t(seminr_model$outer_loadings))==0])^2)/182))
  )

  if(observed){
    result<- list(inner = Q, outer = C, indices = indices)
  }
  else{
    result<- list(outer = C, indices = indices)
  }

  class(result) <- "matrixplsresiduals"
  result
}
