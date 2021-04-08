# # https://rdrr.io/cran/plspm/src/R/rescale.r
#
# library(plspm)
# data(satisfaction)
#
# # define path matrix (inner model)
# IMAG <- c(0,0,0,0,0,0)
# EXPE <- c(1,0,0,0,0,0)
# QUAL <- c(0,1,0,0,0,0)
# VAL <- c(0,1,1,0,0,0)
# SAT <- c(1,1,1,1,0,0)
# LOY <- c(1,0,0,0,1,0)
# sat_path <- rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)
#
# # define list of blocks (outer model)
# sat_blocks <- list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
#
# # vector of modes (reflective indicators)
# sat_modes <- rep("A", 6)
#
# # apply plspm with bootstrap validation
# satpls <- plspm(satisfaction, sat_path, sat_blocks, modes = sat_modes,
#                 scaled = FALSE, boot.val = TRUE)
#
# scores <- satpls$scores[,"IMAG"]
# imag_data <- as.matrix(satpls$data[1:5])
#
# wgs <- satpls$outer_model$weight[1:5]
# wgs/sum(wgs)
# rescaled <- imag_data %*% (wgs/sum(wgs))
#
# as.vector(rescaled)
# rescale(satpls)[,"IMAG"]

#' Rescales standardized construct scores back into same units of items.
#' Inspired by \code{rescale()} function of \code{plspm} package
#' NOTE: Experimental feature only
#'
#' @example
#' measurements <- constructs(
#'   composite("Image",       multi_items("IMAG", 1:5)),
#'   composite("Expectation", multi_items("CUEX", 1:3)),
#'   composite("Loyalty",     multi_items("CUSL", 1:3)),
#'   composite("Complaints",  single_item("CUSCO"))
#' )
#'
#' structure <- relationships(
#'   paths(from = c("Image", "Expectation"), to = c("Complaints", "Loyalty"))
#' )
#'
#' pls_model <- estimate_pls(data = mobi, measurements, structure)
#' unstandardize_scores(pls_model)
#'
#' @export
unstandardize_scores <- function(pls_model) {
  construct_names <- seminr:::construct_names(pls_model$smMatrix)

  construct_names -> .
    sapply(., seminr:::items_of_construct, model=pls_model) -> .
    unlist(.) -> .
    unname(.) -> .
    . -> construct_items

  relevant_data <- pls_model$data[, construct_items]
  weights <- pls_model$outer_weights[names(relevant_data), ]
  weight_sums <- matrix(colSums(weights), ncol(weights), ncol(weights))

  weights %*% (1/weight_sums) -> .
    .[weights == 0] <- 0
    colnames(.) <- colnames(weights)
    . -> normalized_wts

  unstd_scores <- as.matrix(relevant_data) %*% normalized_wts
  as.data.frame(unstd_scores)
}
