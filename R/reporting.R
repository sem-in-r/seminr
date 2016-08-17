# Reporting Functions
print_paths <- function(model_estimated, na.print=".", digits=2) {
  endogenous <- unique(model_estimated$model$strucmod[,"target"])
  exogenous <- unique(model_estimated$model$strucmod[,"source"])
  latent <- model_estimated$model$latent
  structure_spec <- model_estimated$model$D

  # create matrix of relevant path coefficients and NAs otherewise
  path_matrix <- matrix(nrow = length(latent), ncol = length(latent), dimnames = list(latent, latent))
  path_matrix[structure_spec == 1] <- model_estimated$path_coefficients[structure_spec == 1]

  # add R Squared row
  r_sq <- t(rSquared(model_estimated))[1, ]
  path_matrix <- rbind(r_sq, path_matrix)
  rownames(path_matrix) <- c("R^2", latent)

  # round and print
  final_paths <- round(path_matrix[c("R^2", exogenous), endogenous, drop=FALSE], digits)
  print(final_paths, na.print = na.print)
}

plot_scores <- function(fitted_model, factors=NULL) {
  if (missing(factors)) factors <- fitted_model$model$latent

  plot(as.data.frame(fitted_model$factor_scores[, factors]), pch = 16,
       col = rgb(0.5, 0.5, 0.5, alpha = 0.6))
}
