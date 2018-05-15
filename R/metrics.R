# PURPOSE: Functions to compute metrics on measurement or structure

# Function to apply over manifests of a latent and return VIF values
compute_vif <- function(target, predictors, model_data) {
  independents_regr <- stats::lm(paste(target," ~."),
                                 data = as.data.frame(model_data[,predictors]))

  r_squared <- summary(independents_regr)$r.squared
  1/(1 - r_squared)
}
