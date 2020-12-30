# Utilities ----

glue_dot <- function(x) {
  glue::glue(x, .open = "<<", .close = ">>", .envir = parent.frame())
}

#' Wrap a text in single quotes
#'
#' @param x a character string
esc_node <- function(x){
  paste0("'", x ,"'")
}



#' Format p values for the output and removes trailing numbers when p > .10
#' @keywords internal
#' @param pvals A vector with p-values
#' @param sig.limit The lowest threshold for full reporting
#' @param digits the amount of digits to report when sig.limit < p < .10
#'
#' @return A string formated p-value including equal and less-than sign
# @export
#'
# @examples
#' pvalr(c(0.432, 0.05, 0.00001))
pvalr <- function(pvals, sig.limit = .001, digits = 3) {

  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x) #generate sprintf string
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    paste0("= ",res)
  }

  sapply(pvals, function(x, sig.limit) {
    if (x < sig.limit)
      return(sprintf('< %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2))
    else
      return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}
