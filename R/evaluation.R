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
reliability <- function(seminr_model){
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
