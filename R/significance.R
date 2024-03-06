# Functions to guage the significance of bootstrapped results

#' Tests the difference between two path coefficients in a bootstrapped model
#' NOTE: Experimental feature
#'
#' @param path1 A \code{vector} containing \code{from} and \code{to} strings of construct names defining the start and end of that path.
#' @param path1 A \code{vector} containing \code{from} and \code{to} strings of construct names defining the start and end of that path.
#' @param boot_pls Bootstrapped SEMinR model (\code{boot_seminr_model} object)
#'
#' @usage
#'   test_path_diff(path(from="SCVEND", to="TTSE"),
#'                  path(from="SCVEND", to="INER"),
#'                  boot_pls)
#'
#' @export
test_path_diff <- function(path1, path2, boot_pls) {
  lpath <- boot_pls$boot_paths[path1["from"], path1["to"],]
  rpath <- boot_pls$boot_paths[path2["from"], path2["to"],]

  mean1 <- mean(lpath)
  mean2 <- mean(rpath)
  abs_diff <- abs(mean1 - mean2)

  tval <- abs_diff / sqrt(var(lpath) + var(rpath) - 2*cov(lpath, rpath))
  pval <- pt(abs(tval), df=boot_pls$boots-1, lower.tail = FALSE)

  result <- data.frame(mean1 = mean1, mean2 = mean2,
                       abs_diff = abs_diff, tval = tval, pval = pval)

  class(result) <- c(class(result), "seminr_path_diff")
  result
}

#' @export
path <- function(from, to) {
  path_vector <- c(from=from, to=to)
  class(path_vector) <- c(class(path_vector), "seminr_path")

  path_vector
}
