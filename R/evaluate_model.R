## Goodness-of-fit ----

# Calculate insample metrics ----
calc_insample <- function(obsData, construct_scores, smMatrix, dependant, construct_score_cors) {
  # matrix includes BIC
  # Remove BIC for now
  #insample <- matrix(, nrow=3, ncol=length(dependant), byrow =TRUE, dimnames = list(c("Rsq","AdjRsq","BIC"), dependant))

  # matrix excludes BIC
  insample <- matrix(, nrow=2, ncol=length(dependant), byrow =TRUE, dimnames = list(c("Rsq", "AdjRsq"), dependant))

  for (i in 1:length(dependant))  {
    #Indentify the independant variables
    independant <- smMatrix[smMatrix[, "target"]==dependant[i], "source"]

    #Calculate insample for endogenous
    #    construct_score_cors <- stats::cor(construct_scores)
    r_sq <- 1 - 1/solve(construct_score_cors[c(independant, dependant[i]), c(independant, dependant[i])])
    insample[1, i] <- r_sq[dependant[i], dependant[i]]
    insample[2, i] <- 1 - (1 - insample[1, i])*((nrow(obsData)-1)/(nrow(obsData)-length(independant) - 1))
    # Calculate the BIC for the endogenous
    # Remove BIC for now
    #insample[3, i] <- BIC_func(r_sq[dependant[i], dependant[i]], length(independant), nrow(obsData), construct_scores[, dependant[i]])
  }
  return(insample)
}
