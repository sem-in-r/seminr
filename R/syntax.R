# LIBRARIES
library(semPLS)

# FUNCTIONS
# Measurement functions
measure <- function(...) {
  return(matrix(c(...), ncol = 2, byrow = TRUE,
                dimnames = list(NULL, c("source", "target"))))
}

reflect <- function(construct_name, item_name, item_numbers, item_prefix = NULL,
                      item_mid = NULL, item_suffix = NULL) {
  return(as.vector(rbind(rep(construct_name, length(item_numbers)),
                         paste(item_prefix, item_name, item_mid, item_numbers, item_suffix, sep = ""))))
}

form <- function(construct_name, item_name, item_numbers, item_prefix = NULL,
                       item_mid = NULL, item_suffix = NULL) {
  return(as.vector(rbind(paste(item_prefix, item_name, item_mid, item_numbers, item_suffix, sep = ""),
                         rep(construct_name, length(item_numbers)))))
}

single_item <- function(construct_name, item_name) {
  return(as.vector(rbind(construct_name, item_name)))
}

# Structural functions
paths <- function(from, to) {
  return(as.vector(t(as.matrix(expand.grid(from, to)))))
}

structure <- function(...) {
  return(matrix(c(...), ncol = 2, byrow = TRUE,
                dimnames = list(NULL, c("source", "target"))))
}

plot_scores <- function(fitted_model) {
  plot(as.data.frame(fitted_model$factor_scores), pch = 16,
       col = rgb(0.5, 0.5, 0.5, alpha = 0.6))
}
